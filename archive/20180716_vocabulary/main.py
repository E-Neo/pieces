import re
import sys
import sqlite3
import requests
import subprocess
from datetime import datetime
from bs4 import BeautifulSoup


def create_table(cursor):
    cursor.execute('CREATE TABLE IF NOT EXISTS recite_info '
                   '(word VARCHAR(64), commit_time DATETIME, status INT)')
    cursor.execute('CREATE TABLE IF NOT EXISTS vocabulary '
                   '(word VARCHAR(64) PRIMARY KEY, '
                   'chinese VARCHAR(1024), '
                   'description VARCHAR(1024))')
    cursor.execute('CREATE TABLE IF NOT EXISTS word_strength '
                   '(word VARCHAR(64) PRIMARY KEY, '
                   'strength INT)')


def insert(cursor, word, status):
    commit_time = datetime.now().astimezone().isoformat()
    cursor.execute('INSERT INTO recite_info VALUES (?,?,?)',
                   (word, commit_time, status))


def update_word_strength(cursor, word, strength):
    cursor.execute('UPDATE word_strength SET strength = ? WHERE word = ?',
                   (strength, word))


def commit_close(connection):
    connection.commit()
    connection.close()


def spell_correct(word):
    t = subprocess.run(['aspell', 'list'],
                       input=word.encode(), stdout=subprocess.PIPE)
    return len(t.stdout) == 0


def recite(cursor):
    status_pattern = re.compile(r'[01]')
    count = 0
    try:
        while True:
            word = input('word: ').strip()
            if not spell_correct(word):
                print('Please retype that word to make sure it is not a typo!')
                double_check = input('word: ').strip()
                if double_check != word:
                    print('Skipped!')
                    print('--------------------')
                    continue
            status = input('status: ').strip()
            print('--------------------')
            if not status_pattern.fullmatch(status):
                break
            insert(cursor, word, status)
            count += 1
    except KeyboardInterrupt:
        print('total: {}'.format(count))
    print('total: {}'.format(count))


def _review(cursor, query):
    try:
        print('sync...', end='', flush=True)
        sync(cursor)
        print('done')
    except Exception as e:
        print(e)
    cursor.execute(query)
    status_pattern = re.compile(r'[01]')
    rows = cursor.fetchall()
    count = 0
    try:
        for row in rows:
            word = row[0]
            print('word: {}'.format(word))
            input('check: ')
            subprocess.run(['mplayer', '-really-quiet',
                            'audio/{}.mp3'.format(word)],
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
            cursor.execute('SELECT chinese, description FROM vocabulary '
                           'WHERE word = ?',
                           (word,))
            t = cursor.fetchone()
            chinese, description = t if t else ""
            print(chinese)
            print(description)
            status = input('status: ')
            while not status_pattern.fullmatch(status):
                print('Invalid status!')
                status = input('status: ')
            insert(cursor, word, status)
            strength = calculate_strength(cursor, word)
            update_word_strength(cursor, word, strength)
            count += 1
            print('--------------------')
    except KeyboardInterrupt:
        print('total: {}'.format(count))
    print('total: {}'.format(count))


def _list(cursor, query):
    count = 0
    for row in cursor.execute(query):
        word = row[0]
        print(word)
        count += 1
    return count


def look_up(word):
    try:
        r = requests.get('https://cn.bing.com/dict/search?q={}'.format(word))
        soup = BeautifulSoup(r.text, 'html.parser')
        defs = soup.find(class_='qdef').find('ul').find_all('li')[:-1]
        chinese = '\n'.join([x.text for x in defs])
        r = requests.get('https://www.vocabulary.com/dictionary/''{}'
                         .format(word))
        soup = BeautifulSoup(r.text, 'html.parser')
        audio = soup.find(class_='audio')['data-audio']
        d = soup.find(class_='section blurb')
        r = requests.get('https://audio.vocab.com/1.0/us/{}.mp3'.format(audio))
        short_description = d.find(class_='short').text.strip()
        long_description = d.find(class_='long').text.strip()
    except Exception as e:
        short_description, long_description = '', ''
    return (chinese,
            '{}\n{}'.format(short_description, long_description),
            r.content)


def calculate_strength(cursor, word):
    cursor.execute('SELECT status FROM recite_info '
                   'WHERE word = ? ORDER BY commit_time',
                   (word,))
    rows = cursor.fetchall()
    strength = 0
    for row in rows:
        status = row[0]
        if status == 0:
            if strength != 0:
                strength -= 1
        else:
            strength += 1
    return strength


def sync(cursor):
    # sync vocabulary and audio
    cursor.execute('SELECT DISTINCT word FROM recite_info WHERE word NOT IN '
                   '(SELECT word FROM vocabulary)')
    rows = cursor.fetchall()
    for row in rows:
        word = row[0]
        chinese, description, audio = look_up(word)
        cursor.execute('INSERT INTO vocabulary VALUES (?,?,?)',
                       (word, chinese, description))
        with open('audio/{}.mp3'.format(word), 'wb') as f:
            f.write(audio)

    # sync word_strength
    cursor.execute('SELECT DISTINCT word FROM recite_info WHERE word NOT IN '
                   '(SELECT word FROM word_strength)')
    rows = cursor.fetchall()
    for row in rows:
        word = row[0]
        strength = calculate_strength(cursor, word)
        cursor.execute('INSERT INTO word_strength VALUES (?,?)',
                       (word, strength))


def main():
    options = {
        'sync': sync,
        'recite': recite,

        'review_random': lambda cursor:
        _review(cursor,
                "SELECT DISTINCT word FROM recite_info "
                "ORDER BY random() LIMIT 50"),

        'review_within_24hours': lambda cursor:
        _review(cursor,
                "SELECT word FROM "
                "(SELECT word, min(commit_time) as initial_time "
                "FROM recite_info GROUP BY word) "
                "WHERE initial_time >= datetime('now', '-1 day') "
                "ORDER BY random()"),

        'review_forgotten': lambda cursor:
        _review(cursor,
                "SELECT word FROM "
                "(SELECT word, status FROM recite_info "
                "WHERE (word, commit_time) "
                "IN (SELECT word, max(commit_time) FROM recite_info "
                "GROUP BY word)) WHERE status = 0 ORDER BY random()"),

        'review_by_strength': lambda cursor:
        _review(cursor,
                "SELECT word FROM"
                "(SELECT word FROM "
                "(SELECT t1.word, t1.strength, t2.ct FROM word_strength t1 "
                "JOIN "
                "(SELECT word, min(commit_time) AS ct FROM recite_info "
                "GROUP BY word) t2 ON t1.word = t2.word) "
                "ORDER BY strength, ct DESC LIMIT 50) "
                "ORDER BY random()"),

        'review_old': lambda cursor:
        _review(cursor,
                "SELECT word FROM (SELECT word FROM "
                "(SELECT word, max(commit_time) AS last_time "
                "FROM recite_info GROUP BY word) ORDER BY last_time LIMIT 50)"
                "ORDER BY random()"),

        'list_all': lambda cursor:
        _list(cursor,
              "SELECT DISTINCT word from recite_info"),

        'list_within_24hours': lambda cursor:
        _list(cursor,
              "SELECT word FROM "
              "(SELECT word, min(commit_time) as initial_time "
              "FROM recite_info GROUP BY word) "
              "WHERE initial_time >= datetime('now', '-1 day') "),

        'list_forgotten': lambda cursor:
        _list(cursor,
              "SELECT word FROM "
              "(SELECT word, status FROM recite_info "
              "WHERE (word, commit_time) "
              "IN (SELECT word, max(commit_time) FROM recite_info "
              "GROUP BY word)) WHERE status = 0"),

        'list_by_strength': lambda cursor:
        _list(cursor,
              "SELECT word FROM "
              "(SELECT t1.word, t1.strength, t2.ct FROM word_strength t1 "
              "JOIN "
              "(SELECT word, min(commit_time) AS ct FROM recite_info "
              "GROUP BY word) t2 ON t1.word = t2.word) "
              "ORDER BY strength, ct DESC")
    }
    if len(sys.argv) == 1:
        for k in options.keys():
            print(k)
        return
    connection = sqlite3.connect('db.sqlite3')
    cursor = connection.cursor()
    create_table(cursor)
    options[sys.argv[1]](cursor)
    commit_close(connection)


if __name__ == '__main__':
    main()
