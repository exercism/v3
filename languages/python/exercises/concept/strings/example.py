import re

LOGLINE_RE = r"\[(\w*)\] (.*)"


def _extract_pieces(message):
    pieces = re.search(LOGLINE_RE, message)
    return pieces.group(1), pieces.group(2)


def change_log_level(message, new_loglevel):
    """Change loglevel of message to new_loglevel."""
    return f"[{new_loglevel}] {extract_message(message)}"


def extract_message(message):
    _, msg = _extract_pieces(message)
    return msg


def reformat(message):
    loglevel, msg = _extract_pieces(message)
    return f"{msg} ({loglevel.lower()})"
