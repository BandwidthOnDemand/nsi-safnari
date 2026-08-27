package nl.surfnet.safnari

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class UuidSpec extends helpers.Specification:
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

  "The name-based UUID generator" should {
    "generate a deterministic version 3 sequence" in {
      val a = Uuid.nameBasedUuidGenerator("connection-a")
      val b = Uuid.nameBasedUuidGenerator("connection-a")

      val sequenceA = Vector.fill(10)(a())
      val sequenceB = Vector.fill(10)(b())

      sequenceA must beEqualTo(sequenceB)
      sequenceA.forall(uuid => uuid.version == 3 && uuid.variant == 2) must beTrue
    }

    "separate connection IDs with colliding legacy hash codes" in {
      val aId = "eadafc81-0a10-4b4c-b4a4-1a87ab3c7c6f"
      val bId = "024de33b-3866-47ac-9ad3-425001a63a42"

      aId.## must beEqualTo(bId.##)
      Uuid.deterministicUuidGenerator(aId.##)() must beEqualTo(
        Uuid.deterministicUuidGenerator(bId.##)()
      )
      Uuid.nameBasedUuidGenerator(aId)() must not(be_===(Uuid.nameBasedUuidGenerator(bId)()))
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
end UuidSpec
