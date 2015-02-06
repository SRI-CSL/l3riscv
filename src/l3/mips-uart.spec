---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- JTAG UART support
--------------------------------------------------

register JTAG_UART_data :: word
{
  31-16 : RAVAIL    -- Number of characters reamining in read FIFO
     15 : RVALID    -- Indicates whether RW_DATA field is valid
    7-0 : RW_DATA   -- Value to transfer to/from JTAG core
}

register JTAG_UART_control :: word
{
  31-16 : WSPACE    -- Number of spaces available in write FIFO
     10 : AC        -- Indicates that there has been JTAG activity since last
                    -- cleared
      9 : WI        -- Write interrupt is pending
      8 : RI        -- Read interrupt is pending
      1 : WE        -- Write interrupt-enable
      0 : RE        -- Read interrupt-enable
}

record JTAG_UART
{
   base_address      :: bits(37) -- mAddr
   data              :: JTAG_UART_data
   control           :: JTAG_UART_control
   read_fifo         :: byte list
   write_fifo        :: byte list
   read_threshold    :: nat
   write_threshold   :: nat
}

declare JTAG_UART :: JTAG_UART

-------------------------------------------------------------------
-- update interrupt bit
-------------------------------------------------------------------

unit JTAG_UART_update_interrupt_bit () =
{
  readIntr = JTAG_UART.control.RE and JTAG_UART.data.RVALID;
  when JTAG_UART.control.RI <> readIntr do
  {
    JTAG_UART.control.RI  <- readIntr;
    PIC_external_intrs(0)<0> <- readIntr;
    PIC_update (0)
  }
}

-------------------------------------------------------------------
-- load
-------------------------------------------------------------------

unit JTAG_UART_load =
{
   match JTAG_UART.read_fifo
   {
      case Nil =>
      {
         JTAG_UART.data.RAVAIL <- 0; -- should already hold
         JTAG_UART.data.RVALID <- false
      }
      case h @ t =>
      {
         JTAG_UART.data.RW_DATA <- h;
         JTAG_UART.data.RAVAIL <- [Length (t)];
         JTAG_UART.data.RVALID <- true;
         JTAG_UART.read_fifo <- t
      }
   };
   JTAG_UART_update_interrupt_bit ()
}

-------------------------------------------------------------------
-- input
-------------------------------------------------------------------

unit JTAG_UART_input (l::byte list) =
{
   match JTAG_UART.read_fifo : l
   {
      case Nil => JTAG_UART.data.RAVAIL <- 0
      case t =>
      {
         JTAG_UART.read_fifo <- t;
         JTAG_UART.data.RAVAIL <- [Length (t)];
         JTAG_UART.control.AC <- true;
         when not JTAG_UART.data.RVALID do JTAG_UART_load
      }
   };
   JTAG_UART_update_interrupt_bit ()
}

-------------------------------------------------------------------
-- store
-------------------------------------------------------------------

unit JTAG_UART_store (mask::dword, MemElem::dword) =
{
   when mask<63:56> <> 0 and JTAG_UART.control.WSPACE <> 0 do
   {
      JTAG_UART.control.WSPACE <- JTAG_UART.control.WSPACE - 1;
      JTAG_UART.write_fifo <- MemElem<63:56> @ JTAG_UART.write_fifo
   };
   when mask<24> do JTAG_UART.control.RE <- MemElem<24>;
   when mask<25> do JTAG_UART.control.WE <- MemElem<25>;
   when mask<18> and MemElem<18> do JTAG_UART.control.AC <- false;
   JTAG_UART_update_interrupt_bit ()
}

-------------------------------------------------------------------
-- output
-------------------------------------------------------------------

byte list JTAG_UART_output =
{
   JTAG_UART.control.AC <- true;
   l = Reverse (JTAG_UART.write_fifo);
   JTAG_UART.write_fifo <- Nil;
   JTAG_UART.control.WSPACE <- -1;
   JTAG_UART.control.WI <- false;
   JTAG_UART_update_interrupt_bit ();
   return l
}

-------------------------------------------------------------------
-- initialise
-------------------------------------------------------------------

unit JTAG_UART_initialise (uart::nat) =
{
   JTAG_UART.base_address <- [[uart]`40 >>+ 3];
   JTAG_UART.read_threshold <- 0xFF00;
   JTAG_UART.write_threshold <- 0xFFF0;
   JTAG_UART.read_fifo <- Nil;
   JTAG_UART.write_fifo <- Nil;
   JTAG_UART.data.RW_DATA <- 0;
   JTAG_UART.data.RVALID <- false;
   JTAG_UART.data.RAVAIL <- 0;
   JTAG_UART.control.RE <- true;
   JTAG_UART.control.WE <- false;
   JTAG_UART.control.RI <- false;
   JTAG_UART.control.WI <- false;
   JTAG_UART.control.AC <- false;
   JTAG_UART.control.WSPACE <- -1
}
