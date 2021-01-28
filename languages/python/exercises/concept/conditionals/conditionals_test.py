# unit test here
import unittest
from conditionals import *


class TestConditionals(unittest.TestCase):
    # Checking the first condition using assertTrue and assertFalse
    # The values for arguments is not final and should be considered as placeholders
    # More test-cases  required for full testing

    def test_is_criticality_balanced_set1(self):
        self.assertTrue(
            is_criticality_balanced(temprature=750, neutrons_emitted_per_second=650), msg="Expected True but returned False"
        )

    def test_is_criticality_balanced_set2(self):
        self.assertTrue(
            is_criticality_balanced(temprature=799, neutrons_emitted_per_second=501), msg="Expected True but returned False"
        )

    def test_is_criticality_balanced_set3(self):
        self.assertFalse(
            is_criticality_balanced(temprature=500, neutrons_emitted_per_second=600), msg="Expected False but returned True"
        )

    def test_is_criticality_balanced_set4(self):
        self.assertFalse(
            is_criticality_balanced(temprature=800, neutrons_emitted_per_second=500), msg="Expected False but returned True"
        )

# End of first functions testing

# Test case for reactor_efficency()
    # Checking the second condition using assertTrue and assertFalse
    # The values for arguments is not final and should be considered as placeholders
    # More test-cases  required for full testing
    # need to add more info to messages
    # Need to verify if f-string based errors allowed
    def test_reactor_efficency_set1(self):

        test_return = reactor_efficency(
            voltage=100, current=50, theoretical_max_power=5000)
        self.assertEqual(
            test_return, 'green', msg=f"Expected green but returned {test_return}"
        )

    def test_reactor_efficency_set2(self):
        test_return = reactor_efficency(
            voltage=100, current=30, theoretical_max_power=5000)
        self.assertEqual(
            test_return, 'orange', msg=f"Expected orange but returned {test_return}
        )

    def test_reactor_efficency_set3(self):
        test_return = reactor_efficency(
            voltage=100, current=28, theoretical_max_power=5000)
        self.assertEqual(
            test_return, 'red', msg=f"Expected red but returned {test_return}
        )

    def test_reactor_efficency_set4(self):
        test_return = reactor_efficency(
            voltage=100, current=10, theoretical_max_power=5000)
        self.assertEqual(
            test_return, 'black', msg=f"Expected black but returned {test_return}
        )

# End of second function testing

    def test_fail_safe_set1(self):
        pass

    def test_fail_safe_set2(self):
        pass

    def test_fail_safe_set3(self):
        pass

    def test_fail_safe_set4(self):
        pass
