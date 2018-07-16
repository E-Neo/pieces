import re
import sys
import sqlite3
import subprocess
from datetime import datetime


def create_table(cursor):
    cursor.execute('CREATE TABLE IF NOT EXISTS recite_info '
                   '(word VARCHAR(64), commit_time DATETIME, status INT)')


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
            status = input('status: ')
            if status_pattern.fullmatch(status):
                insert(cursor, word, status)
                print('--------------------')
    except KeyboardInterrupt:
        return


def review_within_24hours(cursor):
    _review_common(cursor,
                   "SELECT DISTINCT word FROM recite_info "
                   "WHERE commit_time >= datetime('now', '-1 day')")


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
                   "(SELECT word, sum(status) as strength "
                   "FROM recite_info GROUP BY word) "
                   "ORDER BY strength LIMIT 50")


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
    options[sys.argv[1]](cursor)
    commit_close(connection)


if __name__ == '__main__':
    main()
