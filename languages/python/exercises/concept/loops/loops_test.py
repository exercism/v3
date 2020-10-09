import unittest
from loops import find_failed_student, above_given_marks, first_k_student_marks, centum_scorer


class TestLoops(unittest.TestCase):
    
    def test_find_failed_student_1(self):
        self.assertEqual(
            find_failed_student(student_marks=[40,40,35,70,30]),
            4,
            msg="Number of failed students is invalid",
        )

    def test_find_failed_student_2(self):
        self.assertEqual(
            find_failed_student(student_marks=[90,80,55,70,65]),
            0,
            msg="Number of failed students is invalid",
        )

    def test_above_given_marks_1(self):
        self.assertEqual(
            above_given_marks(student_marks=[90,40,55,70,30], x=100),
            [],
            msg="The Number of marks above given marks is incorrect",
        )

    def test_above_given_marks_2(self):
        self.assertEqual(
            above_given_marks(student_marks=[90,40,55,70,30], x=70),
            [90, 70],
            msg="The Number of marks above given marks is incorrect",
        )

    def test_first_k_student_marks_1(self):
        self.assertEqual(
            first_k_student_marks(student_marks=[90,80,100,70], k=4),
            [90,80,100,70],
            msg="The Number of First K Students are incorrect",
        )

    def test_first_k_student_marks_2(self):
        self.assertEqual(
            first_k_student_marks(student_marks=[90,80,100], k=1),
            [90],
            msg="The Number of First K Students are incorrect",
        )

    def test_centum_scorer_1(self):
        self.assertEqual(
            centum_scorer(student_info={"Charles": 90, "Tony": 80, "Thor": 60}),
            "No Centums",
            msg="Centum Scorer name is wrong",
        )

    def test_centum_scorer_2(self):
        self.assertEqual(
            centum_scorer(student_info={"Charles": 90, "Tony": 80, "Mark": 100}),
            "Mark",
            msg="Centum Scorer name is wrong",
        )
