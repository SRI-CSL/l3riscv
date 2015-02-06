---------------------------------------------------------------------------
-- CHERI capability coprocessor initialisation
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

bool hasCP2 = true

unit COP2Init () =
{
    TAG <- InitMap (false);
    var defaultCapCause :: CapCause;
    defaultCapCause.ExcCode <- 0;
    defaultCapCause.RegNum <- 0;
    capcause <- defaultCapCause;
    var defaultCap :: Capability;
    defaultCap.tag <- true;
    defaultCap.sealed <- false;
    defaultCap.offset <- 0;
    defaultCap.base <- 0;
    defaultCap.length <- ~0;
    defaultCap.otype <- 0;
    defaultCap.perms <- ~0;
    defaultCap.reserved <- 0;
    PCC <- defaultCap;
    for i in 0 .. 31 do CAPR([i]) <- defaultCap
}
