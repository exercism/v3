class Attendee
  def initialize(height)
    @height = height
  end

  def height
    @height
  end

  def pass_id
    @pass_id
  end

  def issue_pass!(pass_id)
    @pass_id = pass_id
  end

  def revoke_pass!
    @pass_id = nil
  end

  # Do not edit above methods, add your own methods below.

  def has_pass?
    raise NotImplementedError, 'Please implement the has_pass? method'
  end

  def fits_ride?(ride_minimum_height)
    raise NotImplementedError, 'Please implement the fits_ride? method'
  end

  def allowed_to_ride?(ride_minimum_height)
    raise NotImplementedError, 'Please implement the allowed_to_ride? method'
  end
end
