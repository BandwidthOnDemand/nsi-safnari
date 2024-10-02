package nl.surfnet.safnari

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class UuidSpec extends helpers.Specification {
  "The deterministic UUID generator" should {
    "generate version 4 (random) and variant 2 (IETF) UUIDs" in prop { (seed: Long) =>
      val generator = Uuid.deterministicUuidGenerator(seed)
      val uuid = generator()

      uuid.version must beEqualTo(4)
      uuid.variant must beEqualTo(2)
    }

    "generate a deterministic sequence from the same seed" in forAll(
      arbitrary[Long],
      Gen.chooseNum(10, 100)
    ) { (seed: Long, count: Int) =>
      val a = Uuid.deterministicUuidGenerator(seed)
      val b = Uuid.deterministicUuidGenerator(seed)

      Vector.fill(count)(a()) must beEqualTo(Vector.fill(count)(b()))
    }
  }

  "The mock UUID generator" should {
    "generate a simple sequence of UUIDs" in forAll(Gen.chooseNum(1, 1000)) { (count: Int) =>
      val generator = Uuid.mockUuidGenerator(start = 1)

      Vector.fill(count)(generator()).map(_.getLeastSignificantBits()) must beEqualTo(
        (1L to count).toVector
      )
    }
  }
}
