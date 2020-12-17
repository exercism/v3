<<<<<<< HEAD
def unexchangeable_value(budget, exchange_rate, spread, denomination):
   exchange_fee = (exchange_rate / 100) * spread
   actual_rate = exchange_rate + exchange_fee
   unexchangeable_amount = (budget / actual_rate) % denomination
   return int(unexchangeable_amount)

print(unexchangeable_value(100000, 10.61, 10, 1))
print(unexchangeable_value(1500, 0.84, 25, 40))
print(unexchangeable_value(425.33, 0.0009, 30, 700))
print(unexchangeable_value(12000, 0.0096, 10, 50))
=======
def estimate_value(budget, exchange_rate):
	return budget / exchange_rate


def get_changes(budget, exchanging_value):
	return budget - exchanging_value


def get_value(denomination, number_of_bills):
	return number_of_bills * denomination


def get_number_of_bills(budget, denomination):
	return int(budget / denomination)


def exchangeable_value(budget, exchange_rate, spread, denomination):
	exchange_fee = (exchange_rate / 100) * spread
	actual_rate = exchange_rate + exchange_fee
	exchangeable_amount = int((budget / actual_rate) / denomination)
	return exchangeable_amount * denomination

def unexchangeable_value(budget, exchange_rate, spread, denomination):
	exchange_fee = (exchange_rate / 100) * spread
	actual_rate = exchange_rate + exchange_fee
	unexchangeable_amount = int((budget / actual_rate) % denomination)
	return unexchangeable_amount
>>>>>>> efbdf69275d9a319919a92bbf25962ad5c91b4cc
