package webjars

import org.bouncycastle.bcpg.{HashAlgorithmTags, PublicKeyAlgorithmTags, PublicKeyPacket, SymmetricKeyAlgorithmTags}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.operator.jcajce.{JcaPGPContentSignerBuilder, JcaPGPDigestCalculatorProviderBuilder, JcaPGPKeyPair, JcePBESecretKeyEncryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKey, PGPSignature}
import webjars.config.AppConfig
import webjars.utils.MavenCentralDeployerLive
import zio.test.*

import java.security.{KeyPairGenerator, Security}
import java.util.{Base64, Date}

object MavenCentralDeployerSpec extends ZIOSpecDefault:

  private def generateKey(): String =
    Security.addProvider(new BouncyCastleProvider())
    val keyPairGenerator = KeyPairGenerator.getInstance("RSA", "BC")
    keyPairGenerator.initialize(2048)
    val keyPair = keyPairGenerator.generateKeyPair()
    val pgpKeyPair = new JcaPGPKeyPair(PublicKeyPacket.VERSION_4, PublicKeyAlgorithmTags.RSA_GENERAL, keyPair, new Date())
    val sha1Calc = new JcaPGPDigestCalculatorProviderBuilder().build().get(HashAlgorithmTags.SHA1)
    val passphrase = "test"
    val identity = "Test User <test@example.com>"
    val signerBuilder = new JcaPGPContentSignerBuilder(pgpKeyPair.getPublicKey.getAlgorithm, HashAlgorithmTags.SHA1)
    val secretKeyEncryptionBuilder = new JcePBESecretKeyEncryptorBuilder(SymmetricKeyAlgorithmTags.CAST5, sha1Calc).setProvider("BC").build(passphrase.toArray)
    val secretKey = new PGPSecretKey(PGPSignature.DEFAULT_CERTIFICATION, pgpKeyPair, identity, sha1Calc, null, null, signerBuilder, secretKeyEncryptionBuilder)
    Base64.getEncoder.encodeToString(secretKey.getEncoded)

  def spec = suite("MavenCentralDeployer")(
    test("asc") {
      val config = TestInfrastructure.testConfig.copy(
        ossGpgKey = Some(generateKey()),
        ossGpgPass = Some("test"),
      )
      val deployer = MavenCentralDeployerLive(config)
      val result = deployer.asc("foo".getBytes)
      assertTrue(result.isDefined)
    },
  ) @@ TestAspect.timeout(zio.Duration.fromSeconds(300))
