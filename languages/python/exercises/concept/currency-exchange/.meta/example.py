def estimate_value(budget, exchange_rate):
	return budget / exchange_rate


def get_changes(budget, exchanging_value):
	return budget - exchanging_value


def get_value(denomination, number_of_bills):
	return number_of_bills * denomination


def get_number_of_bills(budget, denomination):
	return int(budget / denomination)


def exchangable_value(budget, exchange_rate, spread, denomination):
	exchange_fee = (exchange_rate / 100) * spread
	actual_rate = exchange_rate + exchange_fee
	exchangable_amount = int((budget / actual_rate) / denomination)
	return exchangable_amount * denomination