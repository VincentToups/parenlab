function [oparser] = equalgreaterThanstring(s)
%Return a parser which matches the string S only.
oparser = pbind(equalgreaterThanitems(length(s)), @lambdaminus108378);
function [retvalminus108379] = lambdaminus108378(subString)
%
retvalminus108379 = fif(strcmp(subString, s), @()preturn(s), @()@equalnil);

end

end