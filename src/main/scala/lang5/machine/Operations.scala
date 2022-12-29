package lang5.machine

object Operations {

  val Nop: Byte = 0x00
  val Push: Byte = 0x01
  val Pop: Byte = 0x02

  val Jump: Byte = 0x10 // 指定のプログラムアドレスに移動

  val GetPgmPtr: Byte = 0x11 // 現在のプログラムアドレスを取得し、スタックに積む
  val GetFunPtr: Byte = 0x12 // 現在の関数ポインタを取得し、スタックに積む
  val GetStkPtr: Byte = 0x13 // 現在のスタックポインタを取得、スタックに積む
  val SetPgmPtr: Byte = 0x14 // プログラムアドレスを設定
  val SetFunPtr: Byte = 0x15 // 関数ポインタを設定
  val SetStkPtr: Byte = 0x16 // スタックポインタを設定

  val ReadArg: Byte = 0x14
  val SetReturn: Byte = 0x15
  val Return: Byte = 0x16

  val Add: Byte = 0x20

  val End: Byte = 0x7f

}
