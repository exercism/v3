class RidePass
  def issue(height)
    raise NotImplementedError, 'Please implement then #issue method'
  end

  def valid?(pass)
    raise NotImplementedError, 'Please implement the #valid? method'
  end

  def revoke(pass)
    raise NotImplementedError, 'Please implement the #revoke method'
  end
end
