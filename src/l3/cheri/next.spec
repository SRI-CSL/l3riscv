---------------------------------------------------------------------------
-- Next state transition function
-- (c) Alexandre JOANNOU, University of Cambridge
---------------------------------------------------------------------------

--================================================
-- The next state function
--================================================

string log_instruction(w::word, inst::instruction) =
    "instr " : [procID] : " " : [instCnt] : " " :
    hex64(PC) : " : " : hex32(w) : "   " : instructionToString(inst)

unit Next =
{
    clear_logs ();
    currentInst <- None;
    currentInst <- Fetch;
    match currentInst
    {
        case Some (w) =>
        {
            inst = Decode (w);
            mark_log (1, log_instruction(w,inst));
            Run (inst)
        }
        case None => nothing
    };
    match BranchDelay, BranchTo, BranchDelayPCC, BranchToPCC
    {
        case None, None, None, None => when not exceptionSignalled do PC <- PC + 4
        case Some (addr), None, None, None =>
        {
            BranchDelay <- None;
            PC <- addr
        }
        case None, Some (addr), None, None =>
        {
            BranchDelay <- Some (addr);
            BranchTo <- None;
            PC <- PC + 4
        }
        case None, None, Some (addr, cap), None =>
        {
            BranchDelayPCC <- None;
            PC <- addr;
            PCC <- cap
        }
        case None, None, None, Some (addr, cap) =>
        {
            BranchDelayPCC <- Some (addr, cap);
            BranchToPCC <- None;
            PC <- PC + 4
        }
        case _ => #UNPREDICTABLE("Branch follows branch")
    };
    exceptionSignalled <- false;
    CP0.Count <- CP0.Count + 1
}
