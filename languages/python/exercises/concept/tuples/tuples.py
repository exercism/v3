
def get_coordinate(record):
    return record[1]

def convert_coordinate(coordinate):
    # alt     return tuple(coordinate)
    return coordinate[0], coordinate[1]


def compare_records(azara_record, rui_record):
    return  tuple(azara_record[1]) in rui_record

def create_record(azara_record, rui_record):
    if compare_records(azara_record, rui_record):
        return azara_record + rui_record
    else:
        return "not a match"

def clean_up(combined_record_group):
    for item in combined_record_group:
        new_item = item[0], item[2], item[3], item[4]
        print(new_item)

def multiply_records(combined_record_group):
    group_for_three = combined_record_group * 3

    for item in group_for_three:
        print(item)
