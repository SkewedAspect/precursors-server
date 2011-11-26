#!/usr/bin/env python

# Set up the path so we can import `remote`.
from _path import setPath
setPath(__file__)


from StringIO import StringIO

from unittestwrapper import unittest

from remote.cryptors.m2cipher import M2CipherCryptor


class TestM2CypherCryptor(unittest.TestCase):
    def setUp(self):

        #TODO: This just tests aes_128_cbc. Test others.
        self.key = '\xae\xc4V\xa7\x9c\x14\x04=\xc0\x17\xf4\x15>,$8'
        self.iv = '\x85\x82\x80 _\xcbANf\x1f\xd3oR\xd5\x15\x95'

        self.cryptor = M2CipherCryptor(key=self.key, iv=self.iv)

        # 5000 bytes of Lorem ipsum
        self.plaintext = lorem
        self.ciphertext = cipher

    def test_encrypt(self):
        ciphertext = self.cryptor.encrypt(self.plaintext)

        self.assertEqual(ciphertext, self.ciphertext)

    def test_decrypt(self):
        plaintext = self.cryptor.decrypt(self.ciphertext)

        self.assertEqual(plaintext, self.plaintext)

    def test_roundtrip(self):
        ciphertext = self.cryptor.encrypt(self.plaintext)
        plaintext = self.cryptor.decrypt(ciphertext)

        # This is really the only test we care about. If it works, we're good.
        self.assertEqual(plaintext, self.plaintext)

    def test_stream_read(self):
        target = StringIO(self.ciphertext)

        plaintext = M2CipherCryptor(targetStream=target, key=self.key, iv=self.iv).read()

        self.assertEqual(plaintext, self.plaintext)

    def test_stream_write(self):
        target = StringIO()

        cryptor = M2CipherCryptor(targetStream=target, key=self.key, iv=self.iv)
        cryptor.write(self.plaintext)

        self.assertEqual(target.getvalue(), self.ciphertext)

    def test_stream_roundtrip(self):
        target = StringIO()

        cryptor = M2CipherCryptor(targetStream=target, key=self.key, iv=self.iv)
        cryptor.write(self.plaintext)

        # Resets the current position in the stream to the begining.
        target.seek(0)

        plaintext = cryptor.read()

        self.assertEqual(plaintext, self.plaintext)


lorem = """Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris congue, lorem vitae dictum interdum, ligula nunc semper sem, at malesuada lacus justo vel nunc. Etiam id ante quam, ac mattis tortor. Maecenas varius volutpat fringilla. Proin nec massa metus, quis mattis eros. Curabitur in augue dui. Maecenas at purus vitae nibh vulputate volutpat. Sed non lorem a elit sagittis condimentum ac ut sapien. Mauris rutrum, leo non vestibulum condimentum, lectus augue congue lectus, a egestas nisl elit posuere ipsum. Curabitur dolor sem, semper vitae placerat non, molestie sit amet quam.

Phasellus ut dui ligula, non interdum felis. Nulla eleifend lacus elit, et sagittis risus. Sed scelerisque tellus ut tellus aliquet pellentesque at sagittis velit. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Mauris dignissim interdum tortor, nec imperdiet neque molestie pulvinar. Mauris nec leo at velit molestie ultrices. Mauris porta, eros in interdum viverra, arcu dui cursus felis, vitae sollicitudin nunc purus non odio. Fusce suscipit, nulla fringilla sagittis consequat, felis dui adipiscing ipsum, nec ultricies nisi ligula vel ipsum. Nunc at nisl justo, in fermentum turpis. Quisque scelerisque feugiat enim nec commodo. Proin ultrices, odio et dictum sodales, metus odio ultricies orci, nec ullamcorper sapien nibh consectetur est. Sed vehicula fermentum mi in bibendum. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Ut justo orci, aliquam vel venenatis non, feugiat volutpat est.

Curabitur rutrum rhoncus fermentum. Vestibulum iaculis semper diam id adipiscing. Aliquam erat volutpat. Aenean nec mi magna. Nunc vehicula quam ut lacus convallis ullamcorper. Aliquam convallis neque at tellus ultricies id lobortis purus eleifend. Ut id lorem in enim dictum pharetra. Sed nunc ipsum, venenatis ut bibendum vehicula, ultrices sed nibh.

Etiam semper vestibulum ultrices. Etiam posuere, erat vel tincidunt vestibulum, sem lacus vulputate velit, vitae suscipit ipsum velit at nulla. Donec eu nisl felis, a fringilla tortor. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Proin vitae magna tortor, iaculis dictum massa. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Fusce tristique feugiat neque et lacinia. Quisque consequat aliquam luctus. Nam pretium diam vel justo volutpat adipiscing.

Quisque odio erat, scelerisque at accumsan sed, blandit at enim. Quisque tempus tincidunt sapien, eu sagittis lectus lobortis nec. Mauris tellus quam, elementum sed rutrum nec, imperdiet sed est. Quisque vulputate, neque at fermentum tempor, dolor enim luctus metus, quis sollicitudin justo risus eu lacus. Vestibulum vel magna velit, ac semper eros. Suspendisse potenti. Suspendisse elit tellus, convallis et sagittis sit amet, rutrum a lorem. Mauris elementum aliquam sem, eu cursus nisl ullamcorper sit amet. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Aliquam tincidunt lacus leo. Ut malesuada gravida magna in aliquet. Pellentesque euismod imperdiet sodales.

Proin vitae auctor purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Curabitur nisl massa, semper ut sagittis at, eleifend a elit. Donec in lectus erat. Sed in nisl nisi. Nulla viverra dolor ac nisi sodales id viverra dui posuere.

Vestibulum tincidunt mollis elit, id ultrices dolor mattis ac. Ut dictum pharetra massa quis blandit. Suspendisse porta vehicula nulla, ac pretium orci hendrerit id. Donec ut metus eu tellus scelerisque auctor. In hac habitasse platea dictumst. Nunc pellentesque massa vitae diam porta tristique. In pulvinar sem at leo laoreet venenatis. Nunc vitae ipsum libero. Maecenas fermentum quam id sem malesuada ultrices pellentesque ipsum pharetra. Sed sed lorem non lorem interdum luctus eget sed ligula. Nullam a est a tellus pretium hendrerit. Aenean eu libero a neque mattis rutrum. Integer vel turpis lectus. Etiam ullamcorper, nunc malesuada pulvinar sagittis, urna nunc mollis felis, a suscipit enim ipsum at odio. Praesent erat justo, fringilla sed vulputate ac, tempus sed arcu.

Vivamus non ornare lorem. Morbi non elit felis, quis dignissim odio. Vestibulum et augue at nisi varius scelerisque in eget odio. Duis nulla elit, luctus dapibus interdum sit amet, tempor eu ligula. Sed non auctor est. Cras mollis fringilla nulla, nec hendrerit erat posuere sed. Phasellus scelerisque euismod lacus, ac elementum ligula sodales eget.

Morbi consequat, mi non pretium porttitor, velit dui interdum felis, vel suscipit mi diam sit amet justo. Donec tristique velit id mi scelerisque fringilla ut ac diam. Fusce id dolor ut quam dictum condimentum et sed felis. Pellentesque tincidunt cras amet."""

cipher = """\x8c\xb8s\xb9f?\x90H\xc2\xea\xce\nEvl8M\x13\\#\x85V\xe71\x08\xb7|\xb0\x7f\xb5|1\xbf\x0c\xe2\x10m\xc7\x81r\xe3Ff \x05K\xc2\xb2\xd1\xb7\xdaH\xad\xb7\xcf\xe2\xb7;\xdeN\x9fa\xfc\xe4\x1a9\xf8\x9d\x88\xda\xd4\x80k\x05\x80h`\xa0O\xf8j3\xe5J\xeb\xc9\xf4\x03\x8e\x1d!\x1a\xdaP>$\xe3m\xf0\x04F}\x86:\xb8M\xef\x96J\x8d\xe4l \xaa\xac\xe2\xf6P\x075\x03\x06@1R\xbd\x8e\x13X`u\x93:\xf3\xe7\x01\\\x11L\x9c\x08\xda\x9c\xeb\x1fP\x85\xd1\x0b\x1b\xe9\x80\x88S}\x98\x95\x91\xc0\xc1z\xb1c\x026$\xf1\xbb\x15}I\xb8K\x7f\x9d$\x8d\xd4U\x01\xe9\'\x13\xc1~Ko\xd8\xf9t\xef\xadA\x08j\x10\r4\x85v\xde\x83\xe5C\xe5\xd0d4\tV,]\x97\x19\xa9\xec\x81j8\x98O\xc4\xab`\xe8\xf3\x16j\xd5\xbbR\xe4>\x98\x917\xb0|\x80\x87\xe4\xa8!\x1d\x7f\xccu9\xd9\xff`\xef\x9d\x0f\xda\x94/\xe3\x89-\xd5\xcd_&\xe7\xcc\xa83S\xb3|=j\xa1\x1fej\x11\r\xa8\xbc\x08\x7f:P(4\x8e\xbe\x05-\xb96]h\xf71\xb3\x83\xff\xa7c\xc2\x9c\xf2\xe6\xbbZ\x89;hP\xf9\x81\xf8\xdb\x14\xb6\xb79\x9b;\x01"`;\xa0\xf3MI\x83s\x82\x06\t\xa2\x19\x96\xcdY\xa9:R\xe1\x01\\\xaf\xe0\x1c-\x85\xae\x8e7\x1c/\x85kRv\x80\x86\xa6\xe5\xb2K\x95J\x83\x16F\xe2g9SL\xba \x98AG\xd4\xb6\xb5\x0f\xdcc\x1aq\x81\x0f\xdc\x0e<\xa0\xcc\xa0\xd1\xfa\xc5\xa2\xdeI\xdd\xac\xb7x\r2_\x95\x9ac \x9fG7z6\xd8J\xf5\xe2^\x85\xc4\x0f\xd4\x7fG\x05\x91X\x88\x1e\xf9\xec\x94\x96U\xc5\x16Tlf\xf7\x91\xd3\xee\xd3!\x8b\xe5&\xe6)SJ\xf8\t\xaf\xb8\x89\x8f\xa2b\xcb\xce\tM\x83E\x9fB\x13)4n\xbe\xae$\x9a\xbf\xcc\x9e\xd5\x8d3P\x86\x92\xf1y\xa0J\r\xe9?jtg\xf4\xf3\xc3B\xcd\xdf\xe3\xec\xda\xb9\xebIS!\x1a\xfeH\n\xc11\xda\xe2\xa7\x06\xde\x8c\xc9^\xec\xd3,^rx\xde\x0c\xcd\xab\x9c\x03:\x1cC[\xeb\xefd\x88p\xb1\x1e"\x87\xe9\xd6\xd4\xd6\xd2X=[\x02\x86/.\x83U\x0f=\xf8\x1d\r\xae#t!\xff\x15\x11a\xd4\xbdW\x03\xdb\xbbIbm\xa5D\x91\',\xe7_>\xefG\x93\xbd\xef{\x89\xfdEk\x13\x86\xd5\xa53f\xfe\xf6\x81o*Y}?\xb8m{\x98\xb5\x81c\xa2i\x11\x8bx\rEuK\xab\xf9^\xcc\x01N\xf0k\xd6\xccFl\xd3y\xe8\x0c\xde\xaa\xdf&\xcbp\x11\xfaY\xe8:\xb2U\x9c\x91\xc0\xa8\x91\x19pF\xf2V\xc3\xe6t\xe8\xaa\xfd\x80\xb1\xe4\xa88.i?\x11\xde\xb23?G\xc0\xac\x10\x8b\x1ag\xc7I\xb2\xaa\xfc\xd0t\xc4`\x16\x8d\xb0Y\xa2\xb1\xce\xf4(\xd8\t#\xdan\x93\xed\xd7\x17\xafB(\xba)#\xdf\xba\xa0\xff\x94\xc8\xad\x9e\x89!\xa6\x1cRE\xfdF\xa3/\xa08<\x93\x19G\x83\x98\xf2e\x8c\xdb\x972M\xd2\x9c\xf1P\xf5\x85\xe1\xc6\xd4_\xe8u\x86\x01\xc3\xae\xe3\x8cR\xbbaP\xbb\x0e\x8e\xe8(\xe0\xa8\xf4D\x12\xcc\xfa\xe0\xc0\xae|\x01\xe6\x1dE\xfbL5\xa5\x115\xfd{M\x1a\xde1k\xa2}\x97&\xab\xe4I\xed\xb9{\xf9B\xaf\xf0 \xbdk\x829\x7f\xf8\xe4\x9fC\x0c\xaf\x9aC\xae\xc10_\xb7\xe53\x07\xdb\xdb\xf1\xaa\x8f\xd6\tr\xd5\xfd\xa5 \x9f/\x83\x99\x93\x97gOJlN\xaf\xfd^%+k\x18\x89\x1d\xdc\xa4\xb9\x0b\xcf\xbc.cY\x81\xfb\x91\xdf\x85e\x8a\xf9\xbb\xb9\x17\xe8\xe1\xe2\xca\x00/[\xdc\x86\x86\x9f\xb1\xb6\xa8\xb4\xd666\x89\x10hgZrl\xce:E\x15Yac\x89\xf4`t,NI~\xa7X\xaf\xfd\xf0\xb5\xe5R\x83\x14\xf0*\xef\x8e\xa7\x940\x11mWo\xe0\xdd\xe6\xfa\x9c\xbb\xeb\x130q\x01Z\x0f\x03\x14\xbf\x8f\x98\xec\xc0\xc8\xbc\x9a\x7f\x15\xf0\xa8Y1\x14I?\x85\xa2{\x8d@C\xc3\x7f\xd3\xee\x82\xb5\x89\xac Ve\xa7\xd4\xa6\xcf\xed\x81\xc2J\xcf\xf5)\xfd\x1e\x9f\xef\xf4\x1c\xfe\x87\xb8Cn\'\xb0\xcc\x0c\x0e\xd8\xa9z\xf1\xb6\xcc\x07\xf1ot\xd38\x9aP?\xa6\xd7_\xa0\x88N\xf5\xf7\x13\xd3\xfe\x81\x8e\xe1\xccw~\x04\x07 \x94ysr\x81\xf0\xde\xee\x0f>T\x93\x0b\xc7\x9fb\xf8\x8d\x84m0\x8e&a4\x9aR\xb7\xd4\xfb\xd2\x8e>\xfc\xd8\xdd)\xfd\x89\xdaS\x04\x03N\x90JP\xad\x03Z\xf5\xf8/\xc8e\x98\xf3\xacIN\xa9\x1f\xc0\xaa1Af\xb5\xe9\x93\xcb\xa8\x8fsm\xaf6\x9f\t\xae\x99\xdd\x14\x93\xbcJ=h\xd42\xff\xc8\n\xfb)_\xe5\xca\x19\x08\xb8\xccS\xbb\xdd}\x13\x08X`\x03\xe0\xfa\xd5\xb8\x85[\x94\xfc\x1a\x89\x15s\xa5\x9e]\xcb8Fa\xd23\xa6\xf9*3\x96\x9d\xc2\xcc\x91&\x1e\x06(E`\xa9}K\x8f\xe0p\xa3daH\xa3z\x81\xeb\x06\x17\x90\xf8D\x14Os\x85{W\x80!H\x1c?~\xea\x88^\x05\xb61\xfc.\x91~\x16v\xc7\xc5\xe2\xd2\xbe\xc0\xa3\x9b\xec5\x1b-[\xc6\xe0\xab\xe0\xd6\x88\xe1[\x9f\xed\xb7\xb2\r\xe4\xb6zl\x90`\xdb\xc5n%\x8b"\x9e\xb6\x0ft\xc5\x03\xde\x8c\xc0QA\xdd\x14\xcf\xb3tI\x96\x89\x9c\xed\x1d=\xe0\x8a\xa2\x81\xb6\xea\x04\x94\xa7\x80\n\xc4,\xff\\\x01?\xc7\'\xda\xe5\x9fZRZ\xdcu!\x84Q3\n7\x87\xd8]\x1ee\xea6\x0e\xef*~\xef\x9fe\xf3h\x1d\xe5\xd4PPJG\xef\xdd\x02Jfk\xb1\x9bd\x87;\xd5\xd6\x0f\xc0\x8a\xe6v\xe3\xf9\xd5\x11\x93,\x8bn\x1bq\xec\r`\x80\xa3a\x039e\xe9N\x8a\x11\x8bU\xdfyY\x82\x9d\x8a\x8b\xf1P+kw~\xea,\x91c\x10c\xc1rJ-c\x1897 \x84\xc1\xf5g-p\xc2\x0b\xe4o\x10\xc0\xc2\rGJcpw3=\xa3z\xefm\x1a\x10\xda\xfa\xd7\xa5\x96\xb4\x9e\xb3a/\x12\xc0\x10\xb6\x16\xfd\x8c\xf0a@\x00\x07\xa3\xc4&\xfe\xcci\x8dawo\x7f\xbe:A"U+\xbe\x8doR\xcbS\x00\x1f\xc4\xff!n\x91@>\xf5\xbc\xcb\x0b\xcf\xec\xa17 i\x1c\xf0j\x05h\xae\x8c. \x85\x15T\xc8C\x13\x92\xae\xa7ay4Rnu\\\xd0R\xf4\x16\x93\xa5/\xfc\xda\xac\xf7\xb6>R\xfcrY\xa8\xcd\xe9)(j\xc1>`\xb1\xff\xe0\x08\x8f\x05\xa6G\xacawah\xb4\x10\x8c\x81\xc9\xf2\x8b\x15\xe4\xef\xf7\xd37\xe1}\x1c/jO\xa5\x896i\xc5\xa6A\xf6\xc3h\r\x83+\xb6\xb1<\xa2\x13N\x1dl\xc36\xa5\xa0+\x86\xdfB\xa2\xd0\xde\xa5HcQ\xa5\xdd\xb0as\x11?\xfa\xf3\xe8\xf4\xd8\x19\x13\x81\x98\xc6\xd0d\xc6\xff\x1f\x17\xe5g\xdbR\t[\xd9g\xb4\xa4\xc8\xab\xce\x90\xc4\xbaz>\xae\x8e\x19\xa2\x88^\xd0\xa1mq7Y\x8b\xd1Y\x8f\xac\x10\x9d\xaf\x1e\xadO\xff\xdd\xad\x1c\x1d\xef\xf6]\x1c\x87\x8b\xd9~\x86\xcc\xbdQ\xfe\x81\x89\xd9G\x89x\xeaYGL\xbe\xd7\xee\xe4|\xbaA\x80\x10\x19\xd0\xe3\xf8Ts\xd4\x1a@uo\xa0\xc8\xb3\xac\xc7\x8d,\xbe\xec\xb1\x14\xb1\xce\x87\xc3\xef\xe1\\0P9\xd7\xe3\xdb\xd21\xb8\x88\xda\x9a\xe7sT8QH&\xea\xb3\xc4Q\xde\xa0k"\xdbY\xa4~V\x18!\xcc#\xa8\xdf@\xc2\x7f\x83\xd8oS\xc6:\xa5\xf9#\xd2\xca\xa9\x7f\xf1\xab\xc1\xb1}0\x03\xe7\x11x\xe4\xf06\xe6\xf0\t\x85^\x9dI\x01\xa8r\t\xc6\x82\xdb\xdb\xb9\xd82\x82\xbf\x1e\x1b\xa4\xbffY\xa7\xa1\xa2\x14j\xb2]\x14\xc2/\x9a\xf9\x85\xb5]T Y\x95\x97\x03\xccr~Fy\x18\xb9\x05\x8f\x9d*\x88z/\x90\x02\xbc\xf3s\xbf\x14\xfc2\xe9I\x02\x9c1x\xe1O\xae\x80\xf8:\x087\xea\xed\x11hX\x9d\x07\xb3}\xa8\x80\'\x114\xc3\x92\xe6J\xd8\xcc\xcdg\xf6\x95\xec\xea,w\x8b\x9b\xf2\xaaHT\x89$;\x19+\xaf\xaa5"\xba\x12)\x94J\x9a\xb0\xfaF}P\xb9\x18\xd9\xe9\xc8\xed\xcf\xcd*l3\x14\x99\r\xcf\xba8\x9d\x92l\xe4\xf2!\x83\xd4\xc2"\x0e\xaf\xf1\xda\xa3\x8f\x89\xa9Sp"\x87\xe9\x8elc\xc4\xe9 E\x11\xea;\xee\xc5\xfb\x98\xe2\xc5d\x93Q\x0bB\xf6i+\xcd\xc7\xeeC>\xc1\xbc;HG\xb5\x01\x0b\xe8\x96{]\xbew\n\xfc\xd3\x8d\x8c\xcdO\xb7v\x89\x9d\xf7Zc\xf8\x08{\x9a\x94\xf0\x1f:\x1b\x87\xb8\x15d\xbd\x16\\\x1e\xd57\xe6\xce>\x86?\xc0\xc2#\x12UB\\\x0f7\x92a\xacK=}\xe7\\\xedF\x87\x8b\x0ek\x05\x9f\xce\x8f\xf8#\x8c\xf5\xbc+:8\xdb\xfb/\x83\xaa\xc0{\x075\xa0>U\xdb\xcf\xe4\xb9\x9f\x1f\'c O\x995`\xcd\x0c4\xc5=\xfbM\xfb\xf1\xbezjo\x91\xc7\xa1(\x86/\xbb\xc1\xfd\x872\xbd\x0flU\x86\xbd\xa5\x80[\xa8\x1f\xfd\xd3C \xe7eH=*Fj\x1e\xc7\'\xc34H\x81o\xd4_\xdd\x9e\xc3b\xa8V\xeb\xb1\x8bZ\xbc\xf8\xe0\xc9-\x1e\x866\xba\x14v\xd3\xc7\xe0\xc2<\x1f\x8c\xe9\xe3\xfb\xb6\x02d\x9dv\x97\xa3\xc7m\xb1#\xfdb\xa2-Aa\xdb)+l\xc2\x06\x15\xd4\xb5\xd3\xa8\xd0O\x10\xd6\x0b\x9a\x10\xdd5\xb6\x88\x9e\xe9\xa0\xb8\xbf\x83T]\xd9\xfc\xc8\xd6s\xb4\xcb\xaa\x13y\x9a^\x10\x05\xcb\xbb\'\xd9\x07n\x03B\x91Wt\xfaT\xad\xea\x1aQ\xa1\xc9\xa4\xbf\x0c\x89=Jw\xa9\x93\x8dOi\x0c0\x7fZ\xf0\xc5\xd0\xca8;e/\x19!>/p{p\x9eM\'\x8f\x03\x81hm\xf2\xaf\x82p\xec\x86\xdc\x17\x0c3re\xc9\xc9\xed\xe0\x84k\xa6\xbc`\xe0\xba\xf6\xf8%\xe7c\x9d\x81\xdb\x81 n\'{Y\xda\x14c\x0b\xa1\xb7\x92i\xbe\xbf\x00\xcfs\x8dTL\xae`\xbf\\@\xaa\x01\xbdW\xd2\xa8D\x90F\x8d\x87\xf3Z\xe6Fw\x11\xd9\x87\xa4\rJ\xdc\xe2\xd0OP\xb9\rvl/a\x0cf\xcf0\x88\xdar9\xb1)\xa0\xbe<\xa9\xeb\xe6\xcc\x1a\xa3\xeaq\xc4u\n\xe7\xc6\xb3\xcd\xa1,\x8e\x0cj:\xber\x04\xd4\xbe\xde\xc0\xdd\x04\xa3|"\x96\xbe\xa4\x85\x92\xf6_\xc0\x1e*x7\xd8\xe6\xb7\xfaHO\xbb\xe1(\x85v\xc9a\n\xf7\xdar\xb1\r\xa3t\x9b\xeb\x86^\x10\x8aNb`\xb7\xebW@<\x00Zy\x0b\x14P[Y@\x8d\x0c\x16\xe8qI\x1a"\x14\x95\xcc\x00Bj\x90\r\xfc\xc3#\xb46\xdfc\x93\xf6\xad&}\xc1\xcd\xe7\x1f\xb26l\x91I\xd6\xa1\xf5\xda-#\x03\xaf\xabh\xa94\xce\xd3x\xe7O\x06\xe0 \xb5T"\x90;\xca\xb2\x98\x01\xa9\xd7@\xbc4\x90@\x0f\xed\x1c\x94$\x93\x19\xa5\x88\x11H\x993|\xe3\x0c@\x02[.\xe72\x0f\xa94\xb5\xe0\xbc\x80\xd0}"%z\x19[d\xe3\xb0S\x12\xd2G!\x01\x1b%\x9c\\\x1fD\xc4\xa07\xd1h\xfe|R\xda\xf55N)\x1f^\x86\xe8v\xd5\xeb\xdb"\xfdn\xadt\x0e\xc0\x05i\xaf\xac\x8b\xf2\t3\xc1\xc0\xc3d\xb0\xcc\x08\x0b\xe8\x12\x11BM\x90h\x02\xdb\x0c\xf6\x8cY\xb72\x0blw/\xc8k\xdc\xd8\xc2\xf6\xe9\xcd\x92\x84|\x96\x85c\x9e\x10\xceO\xbd\xb1\xd5\x1e\xd6\x8d \xc3\xdd+\xeb\xce\xbdS\xf1N!kT\x19\xa8\xc3\x04r\x8b\x90\xf6\xa3O\x95\x9a$UD=\n\xeb\xa9\x1d\xcc\x9d\x80\xf5W\xc29\xc8\xe9\x92\xda\xc93J\'\xb4B\xda\xc2\x9d\xed\xafC\xddz\x95\xf07\xf9\x05\\U\xf6\xd2\x91\x8f\xf3v\xfbm\x1a\xe0\x83\x07\x95O\x8f\x9f\xf5\x96\xc9\xc9\x14U\xd9\xfc\xa7*\x88\x90\x85o\xc1\xe8X\\\xd8\x1a\xd0\xe1\xb3qX#$\x19\xaa\xb5(\x95x\x7f\xf9b\xa7\x03cM\xfa\xa4Ws<\xb6\x14|qw\x10+\xe56a,q\x97B\xf6\xb3\xfe-%\xf2\x89\x07\x8fe\xb5&K4\x0e@\x03\x08\xcd\xf9\xe2\xb1\xc6\xd6\x964\xc0\xfa\xe7\xe1.\xb1\xaa\xcd\xb8\xb3\x85X_\xb0zvf=\xc6:}/D\x92\x1a\x9fC\xd5\x811\xb0\xd4\x1e\xc1\xa9#\xdcz\xda\x8a>8\xcf(\x8e\x1a\xfeLTFx0\xcc\xf1\xe9h\xec4\xb0\x93\xcd\x02\xc6\xc6Q\xc7$Y YG\x98\x80\x0e\xb6w\xcd\xddK\xe3\xfe[B\xd2\x0f\xe9&\x19G\x1aj\xe9\x1e\xda()\x06!\x1a1\xe8F\xce\x9f~z+\x11*\xd0\xd8]u\x8bh\xe3\xed\x03\xef\xc3\xec\xa2\x00z8\xfd\xf3$\xbc\x99\xc5g\x0b\xb8w\x81\xe1\x97\xc4\xe2!p"\xc2]|\xde\x1bm\x05\xd2\x1c\xa2\xaa\xc1#\xa9S\xb9K\xa8\xbb\x1dM\x89\x17\x87\xef\xdb\x9c\\\x06P\xf4\x82V\xd6\xa9B;{\xbb\xe8\xb6\xd8N\x18\xefY\x81QG\xfes\x1d\x1e\x97\x00}G\xbb\x85\xe6e\x17\xee\xc4\xcbd\x15\xe4\xdf\x8d\xd4}\x81G\xa1y;\x02V\xaf\xd8\x11A\xaf\xbbD`\xc2z\xa2\x03?\xf0B\xa1s||\xf099\xa7\x84+\x8a\xb1Bk\xdc\x8e\xc3\xb2\x9d\xba\x13\x95\xb4\xbd"\xea+{\x0f\xe0\xf6\xc3\xbf\x9b6\xd5\xb4B\xca\x96\nn%\x83\x10\xfaU\xa6@\xdfo#\x92n\xd8\x9e\x95\xff\xd0\xdb\x87\x95\xfeLML\x87\xab\x89\x15\n\xc1\x10\xfc\x9dF\xd2\r\xee\xc4e\xfc\x03Pjx\ne\xa6\xf7I6\xf1p\xc9\xc3\x10,M\xf0\xbeb:\xb0p\x85\x91\xaa\x17\xb9]^\x00\x178\xa6c_\xf51\xb4\xbf\xd1\xb8\xf6h\t\x98\xb7\xc3\x00\x19285,\xceZ\xcb\xdec\x16\x8b\x86\xec\xf8\xf8\x17d\xc9YZ\x0cLf\xc3\x99Z\x8d\x02O.\xef\x94];\xf6-\x95\xfft\x92\x82\xe4\xb0\x02\x05V\xe0\x8b\x07&\xd2x\xa0\xab\xc8\x980\x15\x81D\xdb\xffE\xa6\xe7\xf4\x853\x95\xeb\xeeR\xd0\x8a"K\xc3\xb7=\x14\xf2\x984\x0cg{\x853)\xad\xa8\xc1\x94\xb4\x84\xbf\x87\xbb\xde\x90R\xaep\xf2M\xc66\xb6&\xb2\x86\xd8\x0b\x9f\xfe;h\xf6\'^\xef0\x1e=\xb4c\xa6\xe1\x99%\x0cC"\xc21J\x10@\x99\x85\xad\x18\xe6\xc4C<\x99\x84\x93\xc1\n\xecv\x8bWh\xd1(<\xd0\x94\xa5n\xe5uR\x8c\x01\x8c\xec\xbf\xd5?\xe7U\x0e\xbfu\xce\x8b\xff\xae3\xb1\x802\'\x1fo\xa2\xb8W\xb5q\xd4>1\xf2 &\xd3\xeaL\xf5\xa2\x8a\x0eQ\x16\x05\xa9\xc6\xb8\xe1nhM\xba<\x14gPF\xd2Z\xc4Du\xae2ZO\x88\x8d\xa9\xb5[ej\x01\x13\xfe\x99"\xff\x0e\x196\xc5\x9a]\xfc\xc8\xae\x84\x0c\xa5\x96\xa1\x99M\xf4\x17k\xda\x8a\xf8\x9b\xa0xP[\xaf\xf0\x061\xbaO\xfci\x7fS\xbe\n\xdbk\x9c~ \x9e\x7f\x93V\x1e?\xf9\tZ\xa4\xd2\xfbE\xe1\rJ\xa1<.\xe9\xd7L\xf7\xccg\xed\xe7\xc9\xef\xe3A\xa1\x86\xda\xa9\xe2\x0b*\x02\xa9)\xd4?\x18\xe1\xa8R\x82\x85\x19c\x1fK~\xab\xe3\x1dv\'\x13\x05\xfb\x9d\x0c\xe5\x93\xa8s6\xd1\x84b0\xc6\xcf\x91m0C\x04\x0e\x05VL\xef\xbfWIrK\xf2\x017\xf6ohW\x15\xd5X\x02C!\x97\x9c\xa9^\x10\xd6\xc3\\\xbcb\xcd\xa3\xda\xf4Y$\xe4\x1e\x14`\xc3\x9d\xe6\x9eR\x8bm\xe0\xf8\xfb\xf3\\\xd5Q\x1f\x1dg\xda\x86\xa3i\xd6&\x94Us\x9b\x9c\xf4\x82\x87\xf5\xb1\x81G\xe6R\xc6$\xc8y{\xe0\x12.U\x9d\xc1h5\xe7\x8fo\xac\xa1\x06\\(\x1f?\r\x90\xd3\x18\xaf\xed\xa8]}\xac_W7{\x98\x89\xeb\xffB\xb0`\xa1$\xa3\xd5Z\xa66\x13\xe1fXQ\xe5\x91\xfd\xa7[\xdfX\xec\xb6{\x11\xc8Z\xc9\x10\x15\xeb\xb1Gm#\xb5\xacH\xec\xc2\x1d\x96\x8fb\x02\xf9\x8c\xd8\xe4\xbf\xfc\xa3TWD\x9b\x08=\xb4\x8b\x02\xc6-\xe1g\xb1\xa2\xce\x12\x91<\xf0\xf47\x0f2!\xcb\x97\x8f\xfd?\xd9\x89)|\xbf]|\x92\xc7\xf1\x1b~U\x9cc\x81\x14d\xa7D<\x8as\x82OX\x8c\x10\x03\x03\x08\x03.\xe8\xec\xe9#4\x80d\xb7+\x11\x19\xaelx\n\xcf\xf0\xcc\xcd\x90\xef\xa1J\xf9\x87\xf2oBbt\xd2^\x1e\xec-"\xac=j\xaf|\x0b\xa0\xb9hTp"&L\x96\xcf\xd3\xb2\xb5\xbe\xe7\x11\xb5\xbb\xf4M\xbb&[`\x07\xec\x80\x1c\xdcg\xfa\xe9\xf5Cz\xea^\x9e\x1a\x82u\xbf9\x8c\x99\xe1\xfb\x9b9X\xef\xdfH*U\xdd\xb2\xfd\xc5d\xed\x08\x0c\x13\xf7\xcf\x0bQ\x0b\x1e\\\x8e\xde\xbb\x1e/M\xb8\xf2\x87\x1c\x96d\x002K\xd7\x11\xec5\x07\xd6\x92\xdf\n\xc7\x06V\x86\xe1$J\x17\xbb\xff&\xfeI\xb3\xb5\xaf\xc1\x94\x83M\xc7`\xb9\xbb?W\xa9\x87\x861.r\xf1\xb2\x0f\x18\xf1\xc2\xd5\xf7\xae\xf7\xfd\x07\xaf\x84\x9cD\xf6\x0c\xed\xb6#\x86\xf3a\\!\x89\x04Q\xe3Q\x19t.a\xb6\xd4\x8c\xcc\xb2\xa3}\x7f\xad\x16\xd2\xa6\xa9YY\xb2\xb7\xb6N\xab\x0b\xd0\')\x85\xc5\x1871\xcfb\x88\xc2\xbd\xb9\xe5\xbb^\x1a\x18\x8a1\xad\xd2H\xa9\xa0\xde\xa4\x97\x943\xe4\xe0w\x0b=\r{P\xb5a\x9b\x89\xf9\xda_\xa89`\xcb=\x81N\xf8fl\xd4\xa58^B\xbe\xf2+\xe8f\x06\x0b\x95[\xc4\x14@\xaf#AK?Y\xf2t(q\xd1\tf<d\x9d\xef\x034\x8e\x7f zi\x045\xed\xa7k\x84C\xb3\x96\x86\xac\x11\xf6\xbb\xa7;\x06\xafx\x94RQ\xe4\x9a.>\x81\xee\x18(]\x1a\xbd\x92\x1eF\x16\xd7\x95\xef\xe6\x86W\xe6\x95\xa3\x9e\x9b\xb8\xa0\xa9B\xaa\xe2\x10\x03D(\x1a\xce\x92\x93\xdb+\x92~\xa8\r\x07\xfa\xa7c.\x16\xba5\x1b\x96\x92,\xb4\xc2>g\xe3/=\xb0<\x16\x8b$f\x08Q\xe2\xdf{\x0bI\x04\xb6\xa5\x1a_1d6z\x91\x83g=X\xb4<\xa7\x16\xe8uT&\xe3\xf5ih\x02M\xb1W\x8d\x0b\xd4\xfb\xbchu1\n\x94m\xf5Z\xcf\n:\xa3\xf9^\xfd=\xd3*\x14\xf4\xdd\x8f`H\x07\xa6^v!&\x10|\x05\x9b\xa9\x863\xb4D\x00)\xd4\x02\x1bs\xf4@\xa00\xdfB\x969\x10\xc9\xa1\xa4k\xd9yO\xaf\xe0\x80L)\xd6B\xa8W1A\x02\x0c\x9f?\xfc\x9b\x00\x05h\xf6\x18\x89\xf8\xcb\xca\xfb\x1dw\x88\nI{b\xaeWyz\xd8pnS\xd8\xb5\x9a\x0b\xc9%\xb7\xdeR"\x8e(5R\x1aPW\x9e\xfbtkmP\xc5\x8b\xd96\xe1I\x95\x1e\xe9\xb1Ld6\t\xdd6\xbe\xe0e\xc4\xdc\xe9\xfb\xad\xd1\x07.\xf2\x84\x1d.U^IG\xd4\xb9\x10yG\xe0\xd8\xfa\\\xdf\xe5_\xb8\x1b\xad<\xd3\xa1\n2\x00\x11\x85\xa5\x0f\x9d\xf8uO>\xdcSAYlK\xb2\xce\xb7\x1f#\xbe\xa0\xadQ\x08\xc3\xab1\xb3\xa9L\x1f\x98s\xf7\xccb\nj\x7f$V\xc5\x1b\xa9\xc7A\xe2\x04\xf2\x9a,\xcc\xa3\x9b\x89&\x9a\xf7(\xcc\xb9\xa3\x90\x9a\r|\x86\x13\xc0\xf1\x01\xb5\x99\xba\xa29\xd1\x13\xa3 \xe6z\x10\xe4\xac\x7f`\xefo\x031\xff\xf63\xa9,\x18b\x9ads\x1f\x14\x93\x9f\x86.Xn\xf79\x15;\xadz\xd4z\xc2\xf3\xea\xc1\n=H\xcc\x8eQ0c\xe3\x88\xeb\x112\\\xcc\xa9/\xbaw\xc4\xea>\xa0\xea\r\xf4<\x93\xde6&f\x87;\xc3$>\x03\xd0\x88\x93\xa0\x95\xc6\xdc\xf3\x0b\xca\xb1\xad\xd1k#\xe5CvOR\x06\x1a\xce\r\xd4\xf8or \xd9\xd8\xe86;\x8c\xfe\xfc\xd5r\xd2&\x8avnf;\x85\x9b,:9\x89{S\xe55\x13\xb3\x83\x92`\x1c\xdf\x1f\xa0\x92\x84\x13g\xa1\x18v\xd5\xec\x88@\x8c\xcd7\xe3\x91\xbd}>\x9bs\x9a\xfb\x84\x07\xdd\xa6\xc0\xb5 J\x11\xf5\xf2-\xf7\xbd\x80\x08z\xda"\x96|\xea\xdd\\\x9et\xf3\xb8\x86\xe7\x8d\xa6\xacg\xe5]\xab\x94\xe6\x82\x02$\x18\x8b\xe9\x01\xd2\x05\x1a\x1d\xeb\x08K{a3\x19\xdfgT\x90\xcbk\xc9@<v\xf4M\x98\xb8488\xf4&\xa0\xb7\x86\xf2\xc7\x87f\xa1;\x10\x04\x04\xe3O\x15\xe1r\xa8\x8a\xbc\x06\xf1&\x87\xf3\x8b!\xc0\rBs=y\xf8/\xf1k\xeb\xb2\x96\xe4\xd4\x92\xf5\xd7%p\xec;\xc0r%\xa4\xaf\xf8X\xf4\xde[\xd7Q\xa2\xcc\xf9I\x8d\x84\xacX\xaa\xf0\xf5\xb5pV\x0e\x86#\xba\xee\x95\xc0\xcd\x05)\xca\x00\xc1_.\xe6fYcF\x03\x9a2?(a\xab\x16\x82\x852\x81\xb7\xc1\x1a\xaa\xe6\x8e4\xec\x1fE\x80\xe6\x03\xb2\xe7"""


if __name__ == '__main__':
    # Build the suite of tests
    suite = unittest.TestLoader().loadTestsFromTestCase(TestM2CypherCryptor)

    # Run the tests
    unittest.TextTestRunner(verbosity=2).run(suite)
