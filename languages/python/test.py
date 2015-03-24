import unittest
import hron

class Helpers:
    def get_test_data(fileid):
        with open('..\\..\\reference-data\\' + fileid, 'r', encoding='utf-8-sig') as f:
            return f.readlines()

    def run(tid):
        text = Helpers.get_test_data(tid)
        ctx = hron.DeserializationState(text)
        ctx.enableLogging()
        o = hron.deserialize(ctx)
        log = ctx.actionLog
        logRef = list(map(lambda s: str.rstrip(s, "\r\n"), Helpers.get_test_data(tid + ".actionlog")))
        return (log, logRef)

class DeserializationTests(unittest.TestCase):
    def test_Deserialize_HelloWorld(self):
        (log, logRef) = Helpers.run("helloworld.hron")
        self.assertEqual(log, logRef);

    def test_Deserialize_Simple(self):
        (log, logRef) = Helpers.run("simple.hron")
        self.assertEqual(log, logRef);

    def test_Deserialize_Random(self):
        (log, logRef) = Helpers.run("random.hron")
        self.assertEqual(log, logRef);

    def test_Deserialize_Large(self):
        (log, logRef) = Helpers.run("large.hron")
        self.assertEqual(log, logRef);

if __name__ == '__main__':
    unittest.main()
