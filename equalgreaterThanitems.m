function [oparser] = equalgreaterThanitems(n)
%Return a parser which returns N items from the input.
oparser = @lambdaminus77720;
function [retvalminus77721] = lambdaminus77720(input)
%
retvalminus77721 = fif(lessThanequal(n, length(input)), @()cellArray(input(((1):(n))), input(((plus(n, 1)):(end)))), @()cellArray());

end

end