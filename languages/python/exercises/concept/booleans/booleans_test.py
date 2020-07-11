import unittest
from booleans import eat_ghost, score, lose, win


class TestArcadeGame(unittest.TestCase):

    def ghost_gets_eaten(self):
        self.assertEqual(
            eat_ghost(True, True),
            True,
            msg="ghost should get eaten"
        )

    def ghost_does_not_get_eaten_because_no_power_pellet_active(self):
        self.assertEqual(
            eat_ghost(False, True),
            False,
            msg="ghost does not get eaten because no power pellet active"
        )

    def ghost_does_not_get_eaten_because_not_touching_ghost(self):
        self.assertEqual(
            eat_ghost(True, False),
            False,
            msg="ghost does not get eaten because not touching ghost"
        )

    def score_when_eating_dot(self):
        self.assertEqual(
          score(False, True),
          True,
          msg="score when eating dot"
        )

    def score_when_eating_power_pellet(self):
        self.assertEqual(
            score(True, False),
            True,
            msg="score when eating power pellet"
        )

    def no_score_when_nothing_eaten(self):
        self.assertEqual(
            score(False, False),
            False,
            msg="no score when nothing eaten"
        )

    def lose_if_touching_a_ghost_without_a_power_pellet_active(self):
        self.assertEqual(
            lose(False, True),
            True,
            msg="lose if touching a ghost without a power pellet active"
        )

    def dont_lose_if_touching_a_ghost_with_a_power_pellet_active(self):
        self.assertEqual(
            lose(True, True),
            False,
            msg="don't lose if touching a ghost with a power pellet active"
        )

    def dont_lose_if_not_touching_a_ghost(self):
        self.assertEqual(
            lose(True, False),
            False,
            msg="don't lose if not touching a ghost"
        )

    def win_if_all_dots_eaten(self):
        self.assertEqual(
            win(True, False, False),
            True,
            msg="win if all dots eaten"
        )

    def dont_win_if_all_dots_eaten_but_touching_a_ghost(self):
        self.assertEqual(
            win(True, False, True),
            False,
            msg="don't win if all dots eaten, but touching a ghost"
        )

    def win_if_all_dots_eaten_and_touching_a_ghost_with_a_power_pellet_active(self):
        self.assertEqual(
            win(True, True, True),
            True,
            msg="win if all dots eaten and touching a ghost with a power pellet active"
        )


if __name__ == '__main__':
    unittest.main()
