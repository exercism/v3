import unittest
import example as loglines

class TestLogLines(unittest.TestCase):
    def test_message(self):
        assert loglines.extract_message("[INFO] Hello there.") == "Hello there.", "Should correctly extract a basic message."
