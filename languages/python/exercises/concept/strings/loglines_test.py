import unittest
from loglines import extract_message, change_log_level, reformat


class TestLogLines(unittest.TestCase):
    def test_message(self):
        self.assertEqual(
            extract_message("[INFO] Hello there."),
            "Hello there.",
            "Should correctly extract a basic message.",
        )

    def test_message_with_punctuation(self):
        self.assertEqual(
            extract_message("[WARN] File not found: exercism_practice.py"),
            "File not found: exercism_practice.py",
            "Should preserve punctuation and whitespace from original message.",
        )

    def test_level_word_remains_in_message(self):
        self.assertEqual(
            extract_message("[ERROR] Error while serializing data."),
            "Error while serializing data.",
            "Should preserve a loglevel word that is actually part of the message.",
        )

    def test_change_loglevel(self):
        self.assertEqual(
            change_log_level("[ERROR] No configs found, but not a big deal.", "INFO"),
            "[INFO] No configs found, but not a big deal.",
            "Should replace the loglevel.",
        )

    def test_change_loglevel_with_loglevel_in_message(self):
        self.assertEqual(
            change_log_level("[WARN] Warning: file does not exist.", "INFO"),
            "[INFO] Warning: file does not exist.",
            "Should not replace loglevel names that are part of the message.",
        )

    def test_reformat(self):
        self.assertEqual(
            reformat("[WARN] Warning: file not found."),
            "Warning: file not found. (warn)",
            "Should reformat with lowercase loglevel. ",
        )

    def test_capture_correct_loglevel(self):
        self.assertEqual(
            extract_message("[WARN] Warning: file not found. Will WARN once."),
            "Warning: file not found. Will WARN once.",
            "Should extract loglevel inside brackets, not from messsage.",
        )
