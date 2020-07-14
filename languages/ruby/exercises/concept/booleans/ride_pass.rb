class RidePass
  MINIMUM_HEIGHT = 120

  def issue(height)
    rand(1000) if height > MINIMUM_HEIGHT
  end

  def valid?(pass)
    !pass.nil? && pass != false
  end

  def revoke_pass(pass)
    false unless pass.nil?
  end
end
