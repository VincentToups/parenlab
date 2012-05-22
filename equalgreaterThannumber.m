function [equalp] = equalgreaterThannumber(n)
%Return a parser which succeeds only when it can parse a number from
%the input and that number is N, numerically.
equalp = pbind(@equalnumber, @lambdaminus108382);
function [retvalminus108383] = lambdaminus108382(m)
%
retvalminus108383 = fif(equalequal(n, m), @()preturn(n), @()@equalnil);

end

end