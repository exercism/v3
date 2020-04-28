defmodule SecretsTest do
  use ExUnit.Case

  describe "secret_add" do
    test "add 3" do
      add = Secrets.secret_add(3)
      assert add.(3) === 6
    end

    test "add 6" do
      add = Secrets.secret_add(6)
      assert add.(9) === 15
    end
  end

  describe "secret_subtract" do
    test "subtract 3" do
      subtract = Secrets.secret_subtract(3)
      assert subtract.(6) === 3
    end

    test "subtract 6" do
      subtract = Secrets.secret_subtract(6)
      assert subtract.(3) === -3
    end
  end

  describe "secret_multiply" do
    test "multiply by 3" do
      multiply = Secrets.secret_multiply(3)
      assert multiply.(6) === 18
    end

    test "multiply by 6" do
      multiply = Secrets.secret_multiply(6)
      assert multiply.(7) === 42
    end
  end

  describe "secret_divide" do
    test "divide by 3" do
      divide = Secrets.secret_divide(3)
      assert divide.(6) === 2
    end

    test "divide by 6" do
      divide = Secrets.secret_divide(6)
      assert divide.(7) === 1
    end
  end

  describe "secret_combine" do
    test "add 10 then subtract 5" do
      f = Secrets.secret_add(10)
      g = Secrets.secret_subtract(5)
      h = Secrets.secret_combine(f, g)

      assert h.(5) === 10
    end

    test "multiply by 2 then subtract 20" do
      f = Secrets.secret_multiply(2)
      g = Secrets.secret_subtract(20)
      h = Secrets.secret_combine(f, g)

      assert h.(100) === 180
    end

    test "divide by 10 then add 20" do
      f = Secrets.secret_divide(10)
      g = Secrets.secret_add(10)
      h = Secrets.secret_combine(f, g)

      assert h.(100) === 20
    end

    test "divide by 3 then multiply 5" do
      f = Secrets.secret_divide(3)
      g = Secrets.secret_add(5)
      h = Secrets.secret_combine(f, g)

      assert h.(32) === 15
    end
  end
end
