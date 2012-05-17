function [oparser] = equalgreaterThanitems(n)
%Return a parser which returns N items from the input.
oparser = @lambdaminus82859;
function [retvalminus82860] = lambdaminus82859(input)
%
retvalminus82860 = fif(lessThanequal(n, length(input)), @()cellArray(input(((1):(n))), input(((plus(n, 1)):(end)))), @()cellArray());

end

end