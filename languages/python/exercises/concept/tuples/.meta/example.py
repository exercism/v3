
def get_coordinate(record):
    return record[1]

def convert_coordinate(coordinate):
    # alt return coordinate[0], coordinate[1]
    return tuple(coordinate)

def compare_records(azara_record, rui_record):
    return convert_coordinate(azara_record[1]) in rui_record

def create_record(azara_record, rui_record):
    if compare_records(azara_record, rui_record):
        return azara_record + rui_record
    else:
        return "not a match"

def clean_up(combined_record_group):
    for item in combined_record_group:
        clean_record = item[0], item[2], item[3], item[4]
        print(clean_record)
