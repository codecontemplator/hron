import unittest
import hron

class Helpers:
    def get_test_data(fileid):
        with open('..\\..\\reference-data\\' + fileid, 'r', encoding='utf-8-sig') as f:
            return f.readlines()

    def run(tid, runner):
        text = Helpers.get_test_data(tid)
        ctx = hron.DeserializationState(text)
        ctx.enableLogging()
        o = hron.parse(ctx)
        runner.assertIsNotNone(o)
        log = ctx.actionLog
        logRef = list(map(lambda s: str.rstrip(s, "\r\n"), Helpers.get_test_data(tid + ".actionlog")))
        runner.assertEqual(log, logRef)

class DeserializationTests(unittest.TestCase):
    def test_Deserialize_HelloWorld(self):
        Helpers.run("helloworld.hron", self)

    def test_Deserialize_Simple(self):
        Helpers.run("simple.hron", self)

    def test_Deserialize_Random(self):
        Helpers.run("random.hron", self)

    def test_Deserialize_Large(self):
        Helpers.run("large.hron", self)

if __name__ == '__main__':
    unittest.main()
