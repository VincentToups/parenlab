function f=condHelperFirstTrue(legs)
%%

for li=1:length(legs)
  leg=legs{li};
  pred = leg{1};
  if pred()
    f = leg{2};
    return
  end
end
error('Cond failed to find a true leg.')
