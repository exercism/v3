def change_log_level(message, new_loglevel):
    """Change loglevel of message to new_loglevel."""
    return f"[{new_loglevel}] {extract_message(message)}"

def extract_message(message):
    return message.split("]")[1].strip()
