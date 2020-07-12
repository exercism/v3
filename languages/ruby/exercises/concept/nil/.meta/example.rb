class NameBadge
  def self.print(id, name, department)
    department ||= 'owner'

    prefix = if id.nil? then '' else "[#{id}] - " end
    "#{prefix}#{name} - #{department.upcase}"
  end
end
