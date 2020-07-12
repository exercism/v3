require 'minitest/autorun'
require_relative 'name_badge'

class NameBadgeTest < Minitest::Test
  def test_prints_employee_provided_with_full_data
    assert_equal '[455] - Mary M. Brown - MARKETING',
                 NameBadge.print(455, 'Mary M. Brown', 'MARKETING')
  end

  def test_badge_upcases_the_department
    assert_equal '[89] - Jack McGregor - PROCUREMENT',
                 NameBadge.print(89, 'Jack McGregor', 'Procurement')
  end

  def test_badge_prints_without_id
    assert_equal 'Barbara White - SECURITY',
                 NameBadge.print(nil, 'Barbara White', 'Security')
  end

  def test_prints_the_owner_badge
    assert_equal '[1] - Anna Johnson - OWNER',
                 NameBadge.print(1, 'Anna Johnson', nil)
  end

  def test_prints_the_owner_badge_without_id
    assert_equal 'Stephen Dann - OWNER',
                 NameBadge.print(nil, 'Stephen Dann', nil)
  end
end
