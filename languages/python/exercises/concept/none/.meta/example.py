def find_the_seat(list_of_seats):
    i = 1
    for seat in list_of_seats:
        if seat is None:
            return i
        i += 1

def empty_the_seat(list_of_seats, seat_number):
    list_of_seats[seat_number-1] = None
    return list_of_seats
