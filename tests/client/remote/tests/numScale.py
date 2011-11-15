from decimal import Decimal, getcontext, ROUND_DOWN


def dec_log(self, base=10):
    """Generic logarithm method for Decimal.

    Found at http://www.programmish.com/?p=25

    """
    cur_prec = getcontext().prec
    getcontext().prec += 2
    baseDec = Decimal(10)
    retValue = self

    if isinstance(base, Decimal):
        baseDec = base
    elif isinstance(base, float):
        baseDec = Decimal("%f" % (base))
    else:
        baseDec = Decimal(base)

    integer_part = Decimal(0)

    while retValue < 1:
        integer_part = integer_part - 1
        retValue = retValue * baseDec

    while retValue >= baseDec:
        integer_part = integer_part + 1
        retValue = retValue / baseDec

    retValue = retValue ** 10
    decimal_frac = Decimal(0)
    partial_part = Decimal(1)

    while cur_prec > 0:
        partial_part = partial_part / Decimal(10)
        digit = Decimal(0)

        while retValue >= baseDec:
            digit += 1
            retValue = retValue / baseDec

        decimal_frac = decimal_frac + digit * partial_part
        retValue = retValue ** 10
        cur_prec -= 1

    getcontext().prec -= 2

    return integer_part + decimal_frac


Decimal.log = dec_log


def siPrefix(num):
    scale = (num.logb() / 3).to_integral_value(ROUND_DOWN)

    # The 'mu' character - for "micro"
    mu = u'\u03bc'

    prefix = {
            -8: 'y',
            -7: 'z',
            -6: 'a',
            -5: 'f',
            -4: 'p',
            -3: 'n',
            -2: mu,
            -1: 'm',
            0: '',
            1: 'k',
            2: 'M',
            3: 'G',
            4: 'T',
            5: 'P',
            6: 'E',
            7: 'Z',
            8: 'Y',
            }

    return 1000 ** scale, prefix[scale]


def binaryPrefix(num):
    scale = (num.log(2) / 10).to_integral_value(ROUND_DOWN)

    prefix = {
            0: '',
            1: 'ki',
            2: 'Mi',
            3: 'Gi',
            4: 'Ti',
            5: 'Pi',
            6: 'Ei',
            7: 'Zi',
            8: 'Yi',
            }

    return 1024 ** scale, prefix[scale]


def prefixNum(num, unit, precision=1, binary=False):
    if isinstance(num, int):
        num = Decimal(num)
    elif isinstance(num, float):
        num = Decimal(str(num))

    if binary:
        scale, prefix = binaryPrefix(num)
    else:
        scale, prefix = siPrefix(num)

    return "%.1f %s%s" % (num / scale, prefix, unit)
