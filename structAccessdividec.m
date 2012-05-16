function [b] = structAccessdividec(field)
%Return a function which returns the FIELD of the struct.
b = @lambdaminus170747;
function [retvalminus170748] = lambdaminus170747(theStruct)
%
retvalminus170748 = [ theStruct.(field) ];

end

end