import calendar
from typing import List


class Leaflet:
    def __init__(self, event_name: str, artists: List, unicodes: List):
        self.header = event_name.capitalize()
        self.artists = artists
        self.unicodes = unicodes
        self.date = ''

    def set_date(self, day, month, year=None):
        self.date = f'{calendar.month_name[month]} {day}'
        self.date += f", {year}" if year else ""

    def get_icons(self):
        return [u'{}'.format(number) for number in self.unicodes]

    def get_artists_and_icons(self):
        rows = []
        icons = self.get_icons()
        for i in range(len(self.artists)):
            icon = icons[i] if i < len(icons) else '    '
            rows.append(f'{"":>1}{self.artists[i]:<11}{icon:>3}{"":>2}')
        return rows

    def print(self):
        row_full = ''.join(['*'] * 20)
        empty_row = f'*{"":^18}*'

        poster = []
        poster.append(row_full)
        poster.append(empty_row)
        poster.append(f'*{self.header!r:^18}*')
        poster.append(empty_row)
        poster.append(f'*{self.date!s:^18}*')
        poster.append(empty_row)
        for artist in self.get_artists_and_icons():
            poster.append(f'*{artist}*')
        poster.append(empty_row)
        poster.append(row_full)

        return '\n'.join(poster)


leaflet = Leaflet('Webinar', ['Vince', 'Chris', 'Leo'],
                          ['\U0001F4DA', '\U0001F4BB', '\U0001F3AF'])
leaflet.set_date(22, 6, 2020)
print(leaflet.print())