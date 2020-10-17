def add_me_to_the_queue(express_queue, normal_queue, ticket_type, person_name):
    if ticket_type == 1:
        express_queue.append(person_name)
        return express_queue
    else:
        normal_queue.append(person_name)
        return normal_queue

def add_bunch_to_the_group(queue, people):
    queue.extend(people)
    return queue

def find_his_friend(queue, friend_name):
    return queue.index(friend_name)

def add_person_with_his_friends(queue, index, person_name):
    queue.insert(index, person_name)
    return queue

def remove_the_mean_person(queue, person_name):
    queue.remove(person_name)
    return queue

def how_many_dopplegangers(queue, person_name):
    return queue.count(person_name)

def remove_the_last_person(queue):
    return queue.pop()

def ride_broke_down(queue):
    queue.clear()
    return queue

def new_ride_queue(queue, extra_people):
    new_q = queue.copy()
    new_q.extend(extra_people)
    return new_q

def reverse_the_queue(queue):
    queue.reverse()
    return queue

def sorted_names(queue):
    queue.sort()
    return queue