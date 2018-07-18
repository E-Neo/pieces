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


def insert(cursor, word, status):
    commit_time = datetime.now().astimezone().isoformat()
    cursor.execute('INSERT INTO recite_info VALUES (?,?,?)',
                   (word, commit_time, status))


def commit_close(connection):
    connection.commit()
    connection.close()


def spell_correct(word):
    t = subprocess.run(['aspell', 'list'],
                       input=word.encode(), stdout=subprocess.PIPE)
    return len(t.stdout) == 0


def recite(cursor):
    status_pattern = re.compile(r'[01]')
    try:
        while True:
            word = input('word: ').strip()
            if not spell_correct(word):
                break
            status = input('status: ').strip()
            print('--------------------')
            if not status_pattern.fullmatch(status):
                break
            insert(cursor, word, status)
    except KeyboardInterrupt:
        return


def _review_common(cursor, query):
    cursor.execute(query)
    status_pattern = re.compile(r'[01]')
    rows = cursor.fetchall()
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
            if status_pattern.fullmatch(status):
                insert(cursor, word, status)
                print('--------------------')
    except KeyboardInterrupt:
        return


def review_within_24hours(cursor):
    _review_common(cursor,
                   "SELECT word FROM "
                   "(SELECT word, min(commit_time) as initial_time "
                   "FROM recite_info GROUP BY word) "
                   "WHERE initial_time >= datetime('now', '-1 day')")


def review_forgotten(cursor):
    _review_common(cursor,
                   "SELECT word FROM "
                   "(SELECT word, status FROM recite_info "
                   "WHERE (word, commit_time) "
                   "IN (SELECT word, max(commit_time) FROM recite_info "
                   "GROUP BY word)) WHERE status = 0")


def review_by_strength(cursor):
    _review_common(cursor,
                   "SELECT word FROM "
                   "(SELECT word, min(commit_time) as initial_time, "
                   "sum(status) as strength "
                   "FROM recite_info GROUP BY word) "
                   "ORDER BY strength, initial_time LIMIT 50")


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


def sync(cursor):
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


def main():
    options = {
        'recite': recite,
        'review_within_24hours': review_within_24hours,
        'review_forgotten': review_forgotten,
        'review_by_strength': review_by_strength
    }
    if len(sys.argv) == 1:
        for k in options.keys():
            print(k)
        return
    connection = sqlite3.connect('db.sqlite3')
    cursor = connection.cursor()
    create_table(cursor)
    try:
        print('sync...', end='', flush=True)
        sync(cursor)
        print('done')
    except Exception as e:
        print(e)
    options[sys.argv[1]](cursor)
    commit_close(connection)


if __name__ == '__main__':
    main()
