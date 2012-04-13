function result = fif(condition,truef,falsef)

if condition
  result = truef();
else
  result = falsef();
end