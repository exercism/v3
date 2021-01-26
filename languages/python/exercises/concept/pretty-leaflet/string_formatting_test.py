import unittest
from string_formatting import *


class test_string_formatting(unittest.TestCase):
    def test_header_empty(self):
        self.assertEqual(Leaflet('', [], []).header, '')

    def test_header(self):
        self.assertEqual(Leaflet('Event', [], []).header, 'Event')

    def test_header_capitalized(self):
        self.assertEqual(Leaflet('evENt', [], []).header, 'Event')

    def test_leaflet_date(self):
        self.assertEqual(Leaflet('', [], []).date, '')

    def test_leaflet_date_day_month(self):
        leaflet = Leaflet('Event', [], [])
        leaflet.set_date(15, 12)
        self.assertEqual(leaflet.date, 'December 15')

    def test_leaflet_date_day_month_year(self):
        leaflet = Leaflet('Event', [], [])
        leaflet.set_date(21, 2, 2021)
        self.assertEqual(leaflet.date, 'February 21, 2021')

    def test_leaflet_method_get_icons(self):
        self.assertEqual(Leaflet('', [], []).get_icons(), [])

    def test_leaflet_method_get_icons(self):
        self.assertEqual(
            Leaflet('', [], ['\U0001F98A', '\U0001F340',
                             '\U0001F420']).get_icons(),
            ['🦊', '🍀', '🐠'])

    def test_print_leaflet_concert(self):
        leaflet = Leaflet('Concert', ['John', 'Benjamin', 'Max'],
                          ['\U0001F3B8', '\U0001F3A4', '\U0001F3B9'])
        leaflet.set_date(30, 4)
        handbill = ("""********************
*                  *
*    'Concert'     *
*                  *
*     April 30     *
*                  *
* John         🎸  *
* Benjamin     🎤  *
* Max          🎹  *
*                  *
********************""")
        self.assertEqual(leaflet.print_leaflet(), handbill)

    def test_print_leaflet_teather_empty_date_and_missing_icon(self):
        leaflet = Leaflet('Macbeth', ['Fleance', 'Seyton'], ['\U0001F318'])
        handbill = ("""********************
*                  *
*    'Macbeth'     *
*                  *
*                  *
*                  *
* Fleance      🌘  *
* Seyton           *
*                  *
********************""")
        self.assertEqual(leaflet.print_leaflet(), handbill)

    def test_print_leaflet_webinar(self):
        leaflet = Leaflet('Webinar', ['Vince', 'Chris', 'Leo'],
                          ['\U0001F4DA', '\U0001F4BB', '\U0001F3AF'])
        leaflet.set_date(29, 1, 2020)
        handbill = ("""********************
*                  *
*    'Webinar'     *
*                  *
* January 29, 2020 *
*                  *
* Vince        📚  *
* Chris        💻  *
* Leo          🎯  *
*                  *
********************""")
        self.assertEqual(leaflet.print_leaflet(), handbill)
