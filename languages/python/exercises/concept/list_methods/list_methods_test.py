import unittest
from list_methods import *


class TestListMethods(unittest.TestCase):
    def test_add_me_to_the_queue_set_1(self):
        self.assertListEqual(
            add_me_to_the_queue(express_queue=["IronMan", "BatMan"], normal_queue=["Cyborg", "WW"], ticket_type=1, person_name="RichieRich"),
            ["IronMan", "BatMan", "RichieRich"],
            msg="The person was not added to the queue correctly"
        )

    def test_add_me_to_the_queue_set_2(self):
        self.assertListEqual(
            add_me_to_the_queue(express_queue=["IronMan", "BatMan"], normal_queue=["Cyborg", "WW"], ticket_type=0, person_name="HawkEye"),
            ["Cyborg", "WW", "HawkEye"],
            msg="The person was not added to the queue correctly"
        )

    def test_add_bunch_to_the_group_set_1(self):
        self.assertListEqual(
            add_bunch_to_the_group(queue=["Natasha", "Bruce"], people=["Tchalla", "Wanda", "Rocket"]),
            ["Natasha", "Bruce", "Tchalla", "Wanda", "Rocket"],
            msg="The Bunch of people are incorrectly added"
        )

    def test_add_bunch_to_the_group_set_2(self):
        self.assertListEqual(
            add_bunch_to_the_group(queue=["Natasha", "Bruce", "Tchalla", "Wanda", "Rocket"], people=["Rocket"]),
            ["Natasha", "Bruce", "Tchalla", "Wanda", "Rocket", "Rocket"],
            msg="The Bunch of people are incorrectly added"
        )

    def test_find_his_friend_set_1(self):
        self.assertIs(
            find_his_friend(queue=["Natasha", "SteveRogers", "Tchalla", "Wanda", "Rocket"], friend_name="SteveRogers"),
            1,
            msg="The Index of the friend to find is incorrect"
        )

    def test_find_his_friend_set_2(self):
        self.assertIs(
            find_his_friend(queue=["Natasha", "SteveRogers", "Tchalla", "Wanda", "Rocket"], friend_name="Rocket"),
            4,
            msg="The Index of the friend to find is incorrect"
        )

    def test_add_person_with_his_friends_set_1(self):
        self.assertListEqual(
            add_person_with_his_friends(queue=["Natasha", "SteveRogers", "Tchalla", "Wanda", "Rocket"], index=1, person_name="Bucky"),
            ["Natasha", "Bucky", "SteveRogers", "Tchalla", "Wanda", "Rocket"],
            msg="The People added location were incorrect"
        )

    def test_add_person_with_his_friends_set_2(self):
        self.assertListEqual(
            add_person_with_his_friends(queue=["Natasha", "SteveRogers", "Tchalla", "Wanda", "Rocket"], index=0, person_name="Bucky"),
            ["Bucky", "Natasha", "SteveRogers", "Tchalla", "Wanda", "Rocket"],
            msg="The People added location were incorrect"
        )

    def test_remove_the_mean_person_set_1(self):
        self.assertListEqual(
            remove_the_mean_person(queue=["Natasha", "SteveRogers", "Ultron", "Wanda", "Rocket"], person_name="Ultron"),
            ["Natasha", "SteveRogers", "Wanda", "Rocket"],
            msg="The mean person was not removed properly"
        )

    def test_remove_the_mean_person_set_2(self):
        self.assertListEqual(
            remove_the_mean_person(queue=["Natasha", "SteveRogers", "Ultron", "Wanda", "Ultron", "Rocket"], person_name="Ultron"),
            ["Natasha", "SteveRogers", "Wanda", "Ultron", "Rocket"],
            msg="The mean person was not removed properly"
        )

    def test_how_many_dopplegangers_set_1(self):
        self.assertIs(
            how_many_dopplegangers(queue=["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"], person_name="Bucky"),
            0,
            msg="The Doppleganger count is incorrect"
        )

    def test_how_many_dopplegangers_set_2(self):
        self.assertIs(
            how_many_dopplegangers(queue=["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"], person_name="Natasha"),
            2,
            msg="The Doppleganger count is incorrect"
        )

    def test_remove_the_last_person(self):
        self.assertIs(
            remove_the_last_person(queue=["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"]),
            "Rocket",
            msg="The last person is not removed properly"
        )

    def test_ride_broke_down(self):
        self.assertListEqual(
            ride_broke_down(queue=["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"]),
            [],
            msg="The Queue is not empty"
        )

    def test_new_ride_queue(self):
        old_queue = ["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"]
        comp_queue = old_queue.copy()
        self.assertListEqual(
            new_ride_queue(queue=old_queue, extra_people=["TonyStark", "Pepper"]),
            ["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket", "TonyStark", "Pepper"],
            msg="The New Ride Queue value is incorrect"
        )
        self.assertListEqual(comp_queue, old_queue, msg="Old Queue Value has changed")

    def test_reverse_the_queue(self):
        self.assertListEqual(
            reverse_the_queue(queue=["Natasha", "SteveRogers", "Natasha", "Rocket"]),
            ['Rocket', 'Natasha', 'SteveRogers', 'Natasha'],
            msg="The Queue is not properly reversed"
        )

    def test_sorted_names(self):
        self.assertListEqual(
            sorted_names(queue=["SteveRogers", "Ultron", "Natasha", "Rocket"]),
            ['Natasha', 'Rocket', 'SteveRogers', 'Ultron'],
            msg="The Queue is not properly sorted"
        )
