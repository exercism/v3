def arrange_the_seats(guests):
    seats = dict()
    for seat_num in range(1, guests+1):
        seats[seat_num] = None
    return seats

def is_seat_empty(seat_value):
    if seat_value == None:
        return True
    else:
        return False

def find_the_seat(seats):    
    for seat_num, value in seats.items():
        if is_seat_empty(value):
            return seat_num

def curr_empty_seat_capacity(seats):
    count = 0
    for seat_num, value in seats.items():
        if is_seat_empty(value):
            count = count + 1
    return count

def can_accomodate_seats(seats, guests):
    curr_empty_seats = curr_empty_seat_capacity()
    if guests > curr_empty_seat_capacity:
        return False
    else:
        return True

def empty_the_seat(seats, seat_number):
    seats[seat_number] = None