
# First method, deal with icons.
# Second is date formatting
# Third, deal with the poster.
# Fourth is all together
# I don't have a third yet

import calendar

# class ?


def get_icons(unicode_numbers):
    return [u'{}'.format(number) for number in unicode_numbers]


def get_date(day, month, year=None):
    return f'{calendar.month_name[month]} {day}{f", {year}" if year else ""}'


def get_poster(header, date, icons, artists):
    row_full = ''.join(['*'] * 20)
    empty_row = f'*{"":^18}*'

    poster = []
    poster.append(row_full)
    poster.append(empty_row)
    poster.append(f'*{header!r:^18}*')
    poster.append(empty_row)
    poster.append(f'*{date!s:^18}*')
    poster.append(empty_row)
    for i, a in zip(icons, artists):
        poster.append(f'*{i:>3}{a:>12}{"":>2}*')
    poster.append(empty_row)
    poster.append(row_full)

    return '\n'.join(poster)


print(get_poster('Concert', 'December 3', [
      '🎸', '🎤', '🎹'], ['John', 'Benjamin', 'Max']))

print(get_date(6, 12, 1991))
print(get_date(6, 12))

print(get_icons(['\U0001f3b8']))

"""
********************
*                  *
*    'Concert'     *
*                  *
*    December 3    *
*                  *
*  🎸        John  *
*  🎤    Benjamin  *
*  🎹         Max  *
*                  *
********************
"""
