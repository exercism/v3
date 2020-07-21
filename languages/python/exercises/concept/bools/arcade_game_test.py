import unittest
from arcade_game import eat_ghost, score, lose, win


class TestArcadeGame(unittest.TestCase):

    def test_ghost_gets_eaten(self):
        self.assertIs(
            eat_ghost(True, True),
            True,
            msg="ghost should get eaten"
        )

    def test_ghost_does_not_get_eaten_because_no_power_pellet_active(self):
        self.assertIs(
            eat_ghost(False, True),
            False,
            msg="ghost does not get eaten because no power pellet active"
        )

    def test_ghost_does_not_get_eaten_because_not_touching_ghost(self):
        self.assertIs(
            eat_ghost(True, False),
            False,
            msg="ghost does not get eaten because not touching ghost"
        )

    def test_score_when_eating_dot(self):
        self.assertIs(
          score(False, True),
          True,
          msg="score when eating dot"
        )

    def test_score_when_eating_power_pellet(self):
        self.assertIs(
            score(True, False),
            True,
            msg="score when eating power pellet"
        )

    def test_no_score_when_nothing_eaten(self):
        self.assertIs(
            score(False, False),
            False,
            msg="no score when nothing eaten"
        )

    def test_lose_if_touching_a_ghost_without_a_power_pellet_active(self):
        self.assertIs(
            lose(False, True),
            True,
            msg="lose if touching a ghost without a power pellet active"
        )

    def test_dont_lose_if_touching_a_ghost_with_a_power_pellet_active(self):
        self.assertIs(
            lose(True, True),
            False,
            msg="don't lose if touching a ghost with a power pellet active"
        )

    def test_dont_lose_if_not_touching_a_ghost(self):
        self.assertIs(
            lose(True, False),
            False,
            msg="don't lose if not touching a ghost"
        )

    def test_win_if_all_dots_eaten(self):
        self.assertIs(
            win(True, False, False),
            True,
            msg="win if all dots eaten"
        )

    def test_dont_win_if_all_dots_eaten_but_touching_a_ghost(self):
        self.assertIs(
            win(True, False, True),
            False,
            msg="don't win if all dots eaten, but touching a ghost"
        )

    def test_win_if_all_dots_eaten_and_touching_a_ghost_with_a_power_pellet_active(self):
        self.assertIs(
            win(True, True, True),
            True,
            msg="win if all dots eaten and touching a ghost with a power pellet active"
        )
