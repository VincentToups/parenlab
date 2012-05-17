function [equalp] = equalgreaterThannumber(n)
%Return a parser which succeeds only when it can parse a number from
%the input and that number is N, numerically.
equalp = pbind(@equalnumber, @lambdaminus82865);
function [retvalminus82866] = lambdaminus82865(m)
%
retvalminus82866 = fif(equalequal(n, m), @()preturn(n), @()@equalnil);

end

end