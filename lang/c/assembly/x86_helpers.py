#!/usr/bin/env python3
#
from ctypes import c_int8
from ctypes import c_uint8
from ctypes import c_int16
from ctypes import c_uint16
from ctypes import c_int32
from ctypes import c_uint32
from ctypes import c_int64
from ctypes import c_uint64
from ctypes import c_float
from ctypes import c_double
from collections import namedtuple

class cpu_int:
    def __repr__(self):
        return f'{self._val.value:x}'

    def bit(self, idx):
        return bool((self._val.value >> idx) & 0x01)

    def _bits(self, len):
        bs = []
        for i in range(0, len):
            b = bool((self._val.value >> i) & 0x01)
            bs.append(b)

        return bs


class u8(cpu_int):
    def __init__(self, val):
        self._val = c_uint8(val)

    @property
    def bits(self):
        return self._bits(8)

class i8(cpu_int):
    def __init__(self, val):
        self._val = c_int8(val)

    @property
    def bits(self):
        return self._bits(8)

class u16(cpu_int):
    def __init__(self, val):
        self._val = c_uint16(val)

    @property
    def bits(self):
        return self._bits(16)

class i16(cpu_int):
    def __init__(self, val):
        self._val = c_int16(val)

    @property
    def bits(self):
        return self._bits(16)

class u32(cpu_int):
    def __init__(self, val):
        self._val = c_uint32(val)

    @property
    def bits(self):
        return self._bits(32)

class i32(cpu_int):
    def __init__(self, val):
        self._val = c_int32(val)

    @property
    def bits(self):
        return self._bits(32)

class u64(cpu_int):
    def __init__(self, val):
        self._val = c_uint64(val)

    @property
    def bits(self):
        return self._bits(64)

class i64(cpu_int):
    def __init__(self, val):
        self._val = c_int64(val)

    @property
    def bits(self):
        return self._bits(64)

class flags(cpu_int):
    def __init__(self, val):
        self._val = c_uint32(val)

    @property
    def flag_position(self):
        return {
            "CF": 0,
            "PF": 2,
            "AF": 4,
            "ZF": 6,
            "SF": 7,
            "TF": 8,
            "IF": 9,
            "DF": 10,
            "OF": 11,
            "IOPL": [12, 13],
            "NT": 14,
            "RF": 16,
            "VM": 17,
            "AC": 18,
            "VIF": 19,
            "VIP": 20,
            "ID": 21,
        }


    def get_flag_at(self, pos):
        return bool((self._val.value >> pos) & 0x01)

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return repr(self.bits)

    @property
    def bits(self):
        t = namedtuple("EFlags", ["CF", "U1", "PF", "U3", "AF", "U5",
                               "ZF", "SF", "TF", "IF", "DF", "OF",
                               "IOPL0", "IOPL1", "NT", "U15", "RF", "VM", "AC",
                               "VIF", "VIP", "ID", "U22", "U23", "U24",
                               "U25", "U26", "U27", "U28", "U29", "U30", "U31"])
        tup = t(
            CF = self.CF,
            U1= True,
            PF = self.PF,
            U3 = False,
            AF = self.AF,
            U5 = False,
            ZF = self.ZF,
            SF = self.SF,
            TF = self.TF,
            IF = self.IF,
            DF = self.DF,
            OF = self.OF,
            IOPL0 = bool(self.IOPL & 0x01),
            IOPL1 = bool(self.IOPL & 0x02),
            NT = self.NT,
            U15 = False,
            RF = self.RF,
            VM = self.VM,
            AC = self.AC,
            VIF = self.VIF,
            VIP = self.VIP,
            ID = self.ID,
            U22 = False,
            U23 = False,
            U24 = False,
            U25 = False,
            U26 = False,
            U27 = False,
            U28 = False,
            U29 = False,
            U30 = False,
            U31 = False
        )
        return tup

    @property
    def CF(self):
        return self.get_flag_at(self.flag_position["CF"])

    @property
    def PF(self):
        return self.get_flag_at(self.flag_position["PF"])

    @property
    def AF(self):
        return self.get_flag_at(self.flag_position["AF"])

    @property
    def ZF(self):
        return self.get_flag_at(self.flag_position["ZF"])

    @property
    def SF(self):
        return self.get_flag_at(self.flag_position["SF"])

    @property
    def TF(self):
        return self.get_flag_at(self.flag_position["TF"])

    @property
    def IF(self):
        return self.get_flag_at(self.flag_position["IF"])

    @property
    def DF(self):
        return self.get_flag_at(self.flag_position["DF"])

    @property
    def OF(self):
        return self.get_flag_at(self.flag_position["OF"])

    @property
    def IOPL(self):
        return self._val.value >> 12 & 0x03

    @property
    def NT(self):
        return self.get_flag_at(self.flag_position["NT"])

    @property
    def RF(self):
        return self.get_flag_at(self.flag_position["RF"])

    @property
    def VM(self):
        return self.get_flag_at(self.flag_position["VM"])

    @property
    def AC(self):
        return self.get_flag_at(self.flag_position["AC"])

    @property
    def VIF(self):
        return self.get_flag_at(self.flag_position["VIF"])

    @property
    def VIP(self):
        return self.get_flag_at(self.flag_position["VIP"])

    @property
    def ID(self):
        return self.get_flag_at(self.flag_position["ID"])

def get_physical_address(segment: int, offset: int):
    return (segment << 4) + offset

def addr(segment, offset):
    a = get_physical_address(segment, offset)
    return f'0x{a:x}'
