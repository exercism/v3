class BoutiqueInventory
  def initialize(items)
    @items = items
  end
  
  def item_names
    raise NotImplementedError
  end

  def cheap
    raise NotImplementedError
  end

  def out_of_stock
    raise NotImplementedError
  end

  def stock_for_item
    raise NotImplementedError
  end

  def total_stock
    raise NotImplementedError
  end

  private
  attr_reader :items
end
