



new.vars = names(new)
# "documentno", "type",  "accidentdate", 'mineid'

new$accidentdate = 
  new$mineid = NULL

old.vars = names(old)
#'documentno', 'type'

new.vars = gsub("_", ".", new.vars)
old.vars = gsub("_", ".", old.vars)
old.vars = gsub("actvity", "actvty", old.vars)
new.vars = gsub("actvity", "actvty", new.vars)

setdiff(new.vars, old.vars)
setdiff(old.vars, new.vars)

safe = new

