-- Function to calculate factorial
function factorial(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n - 1)
    end
end

-- Test the function
local number = 5
print("Factorial of " .. number .. " is " .. factorial(number))