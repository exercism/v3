module SavingsAccount
  RATE = {
    NEGATIVE: -3.213,
    SMALL_POSITIVE: 0.5,
    MEDIUM_POSITIVE: 1.621,
    LARGE_POSITIVE: 2.475
  }

  def self.interest_rate(balance)
    if balance.negative?
      RATE[:NEGATIVE]
    elsif balance < 1000
      RATE[:SMALL_POSITIVE]
    elsif balance < 5000
      RATE[:MEDIUM_POSITIVE]
    else
      RATE[:LARGE_POSITIVE]
    end
  end

  def self.annual_balance_update(balance)
    balance + annual_yield(balance)
  end

  def self.years_before_desired_balance(current_balance, desired_balance)
    years = 0
    while current_balance < desired_balance
      current_balance = annual_balance_update(current_balance)
      years += 1
    end
    years
  end

  private

  def self.annual_yield(balance)
    multiplier = interest_rate(balance) / 100
    balance.abs * multiplier
  end
end
