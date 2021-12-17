#lang brag

bits-prog: Packet* padding

zero: "0"
one: "1"
bit: zero | one

padding: zero*
byte: bit{4}
fifteenBits: bit{15}
elevenBits: bit{11}

version: bit{3}


BIN-FOUR: one zero zero
OpTypeID: (zero bit{2}) | (one one bit) | (one bit one)

lengthTypeID: one | zero


Packet: version (LiteralPacket | OperatorPacket) 

LiteralPacket: BIN-FOUR LiteralValue
LiteralValue: NonTerminalValueGroup* TerminalValueGroup
NonTerminalValueGroup: one byte
TerminalValueGroup: zero byte

OperatorPacket: OpTypeID SubPacketInfo
SubPacketInfo: (zero fifteenBits) | (one elevenBits)                          