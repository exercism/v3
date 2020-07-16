class RidePass
  MINIMUM_HEIGHT = 106

  def issue(height)
    rand(100) if height >= MINIMUM_HEIGHT
  end

  def valid?(pass)
    !!pass
  end

  def revoke(pass)
    false unless pass.nil?
  end
end
