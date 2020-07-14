require 'minitest/autorun'
require_relative 'ride_pass'

class NameBadgeTest < Minitest::Test
  def test_pass_number_issued_for_acceptable_height
    assert RidePass.new.issue(125)
  end

  def test_pass_not_issued_for_unacceptable_height
    refute RidePass.new.issue(115)
  end

  def test_unissued_pass_is_nil_as_does_not_exist
    assert RidePass.new.issue(115).nil?
  end

  def test_issued_pass_is_valid
    issuer = RidePass.new
    pass = issuer.issue(125)
    assert issuer.valid?(pass)
  end

  def test_unissued_pass_is_not_valid
    issuer = RidePass.new
    pass = issuer.issue(115)
    refute issuer.valid?(pass)
  end

  def test_issued_then_revoked_pass_is_false
    issuer = RidePass.new
    pass = issuer.issue(125)
    assert_equal false, issuer.revoke_pass(pass)
  end

  def test_unissued_then_revoked_pass_is_nil
    issuer = RidePass.new
    pass = issuer.issue(125)
    assert_equal false, issuer.revoke_pass(pass)
  end
end
