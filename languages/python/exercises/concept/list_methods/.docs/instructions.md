You own a theme park. And for some reason, you own only one ride. The biggest roller coaster in the world. Although, you have only one ride, people stand in line for hours for the ride.

You do have 2 queues for this ride.

1. Normal Queue
2. Express Queue ( Also known as Fast-track Queue) - people pay extra to get faster access to the ride.

You are given a tasks to finish the following functions for the completion of this exercise.

## 1. Add Me to the queue

Define the `add_me_to_the_queue()` function that takes 4 parameters `express_queue, normal_queue, ticket_type, person_name` and returns the queue to which the person is added.

1. express queue is a list
2. normal queue is a list
3. ticket_type is a int in which 1 means he belongs to express queue, 0 means he belongs to normal queue.
4. person name is the name of the person to be added to the respective queue.

Once you have added the name to the respective queue, returned the added queue,

```python
add_me_to_the_queue(express_queue=["IronMan", "BatMan"], normal_queue=["Cyborg", "WW"], ticket_type=1, person_name="RichieRich")
# => ["IronMan", "BatMan", "RichieRich"]

add_me_to_the_queue(express_queue=["IronMan", "BatMan"], normal_queue=["Cyborg", "WW"], ticket_type=0, person_name="HawkEye")
# => ["Cyborg", "WW", "HawkEye"]
```

## 2. We came as a Group

Suddenly, a bunch of people has come together to join the ride. They wish to join the same queue. Your job is to define the function `add_bunch_to_the_group()` which takes 2 parameters `queue, people`.

1. queue is the list where the bunch of people has to be added.
2. people is the list who wants to join the queue.

```python
add_bunch_to_the_group(queue=["Natasha", "Bruce"], people=["Tchalla", "Wanda", "Rocket"])
# => ["Natasha", "Bruce", "Tchalla", "Wanda", "Rocket"]
```

## 3. Where are my friends

One guy came late to the park and wants to join with his friends. He doesn't know where they are standing but wants to join with them.

Define the `find_his_friend()` function that takes 2 parameters `queue, friend_name`.

1. Queue is the list of people standing in the queue.
2. friend_name is the name of the friend who's index you need to find.

Indexing starts from 0.

```python
find_his_friend(queue=["Natasha", "SteveRogers", "Tchalla", "Wanda", "Rocket"], friend_name="SteveRogers")
# => 1
```

## 4. Can I please join with them?

Define the `add_person_with_his_friends()` function that takes 3 parameters `queue, index, person_name`.

1. Queue is the list of people standing in the queue.
2. index is the location the person has to be added
3. person_name is the name of the person to add at that index.

```python
add_person_with_his_friends(queue=["Natasha", "SteveRogers", "Tchalla", "Wanda", "Rocket"], index=1, person_name="Bucky")
# => ["Natasha", "Bucky", "SteveRogers", "Tchalla", "Wanda", "Rocket"]
```

## 5. Mean person in the queue

You just heard from the queue that there is a mean person in the queue and you need to throw him out.

Define the `remove_the_mean_person()` function that takes 2 parameters `queue, person_name`.

1. Queue is the list of people standing in the queue.
2. person_name is the name of the person whom we need to kick out.

```python
remove_the_mean_person(queue=["Natasha", "SteveRogers", "Ultron", "Wanda", "Rocket"], person_name="Ultron")
#=> ["Natasha", "Bucky", "SteveRogers", "Wanda", "Rocket"]
```

# 6. DoppelGangers

You may not have seen 2 people look like the same person before but you sure would have seen people with the same name. It looks like today you are seeing the same scenario. You want to know how many times a name has occured in the queue.

Define the `how_many_dopplegangers()` function that takes 2 parameters `queue, person_name`.

1. Queue is the list of people standing in the queue.
2. person_name is the name of the person whom you think have been occuring more than once in the queue.

Return the number of occurances of the name, in `int` format.

```python
how_many_dopplegangers(queue=["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"], person_name="Natasha")
#=> 2
```

## 7. Remove the last guy

There is an overcrowd in the queue and you need to remove the last person out. You will have to define the function `remove_the_last_person()` that takes 1 parameter `queue` which will be called again and again till the queue count is back to normal. Queue is the list of people standing in the queue.

You should also return the name of the person who was removed.

```python
remove_the_last_person(queue=["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"])
#=> Rocket
```

## 8. The Ride broke down

This day is very uneventful, the ride has broken down. You need to send everyone home, you have to clear everyone from the queue.

Define the `ride_broke_down()` function that takes 1 parameter `queue`.

Return the Cleared queue list.

```python
ride_broke_down(queue=["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"])
#=> []
```

## 9. New Ride Begins

You are careful now that having one ride alone is not good for your business. you open another ride with similar experience. people want to go on both rides. You need to create a new queue and also add other people who are interested in the new ride.

Define the `new_ride_queue()` function that takes 2 parameters `queue and extra_people`.

1. The queue from which you need to copy the names of the people to a new queue.
2. The extra set of people names who need to be added in the new queue.

Note: the extra people does not want to go to the old ride (or the old queue). So you should not add them to the old queue.

Return the new queue.

```python
new_ride_queue(queue=["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"], extra_people=["TonyStark", "Pepper"])
#=> ["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket", "TonyStark", "Pepper"]
```

## 10. The Queue is in reverse

While creating the new queue for the new ride, you have accidently reversed the order of the people names.

Define the `reverse_the_queue()` function that takes 1 parameter `queue`. Queue is the list of people standing in the queue.

```python
reverse_the_queue(queue=["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"])
#=>['Rocket', 'Natasha', 'Ultron', 'SteveRogers', 'Natasha']
```

## 11. Sort the Queue List

For Admin Purpose, you need the list of names in the queue in sorted order.

Define the `sorted_names()` function that takes 1 parameter `queue`. Queue is the list of people standing in the queue.

```python
sorted_names(queue=["Natasha", "SteveRogers", "Ultron", "Natasha", "Rocket"])
#=>['Natasha', 'Natasha', 'Rocket', 'SteveRogers', 'Ultron']
```
