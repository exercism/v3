def unexchangeable_value(budget, exchange_rate, spread, denomination):
   exchange_fee = (exchange_rate / 100) * spread
   actual_rate = exchange_rate + exchange_fee
   unexchangeable_amount = (budget / actual_rate) % denomination
   return int(unexchangeable_amount)

print(unexchangeable_value(100000, 10.61, 10, 1))
print(unexchangeable_value(1500, 0.84, 25, 40))
print(unexchangeable_value(425.33, 0.0009, 30, 700))
print(unexchangeable_value(12000, 0.0096, 10, 50))
