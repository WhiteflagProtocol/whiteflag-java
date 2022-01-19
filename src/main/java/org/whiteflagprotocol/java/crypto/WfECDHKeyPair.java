/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import java.math.BigInteger;
import java.security.Security;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.GeneralSecurityException;
import java.security.interfaces.ECPrivateKey;
import java.security.interfaces.ECPublicKey;

import javax.crypto.KeyAgreement;
import javax.security.auth.Destroyable;
import javax.security.auth.DestroyFailedException;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.jce.ECNamedCurveTable;
import org.bouncycastle.jce.spec.ECNamedCurveParameterSpec;
import org.bouncycastle.jce.spec.ECPrivateKeySpec;
import org.bouncycastle.jce.spec.ECPublicKeySpec;
import org.bouncycastle.math.ec.ECCurve;
import org.bouncycastle.math.ec.ECPoint;

/* Static import of cryptographic utility functions */
import static org.whiteflagprotocol.java.crypto.WfCryptoUtil.convertToByteArray;

/**
 * Whiteflag ECDH Key Pair class
 *
 * <p> This class represents an Elleptic Curve Diffie-Hellmann key pair
 * used by Whiteflag for cryptographic key negotiation. The elliptic curve
 * parameters that must be used for Whiteflag are defined by the
 * brainpoolP256r1 curve as specified in RFC 5639. Public keys are shared
 * as raw 264-bit compressed public ECDH keys.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.2 Key Agreement
 * 
 * @since 1.1
 */
public final class WfECDHKeyPair implements Destroyable {

    /* STATIC CLAUSE */
    static {
        /* Load cryptographic provider */
        Security.addProvider(new BouncyCastleProvider());
    }

    /* PROPERTIES */

    /* Constants */
    /**
     * The name of the elleptic curve used by Whiteflag for ECDH hey negotiation
     * @wfref 5.2.2 Key Agreement
     */
    public static final String CURVENAME = "brainpoolP256R1";

    /* Cryptographic parameters */
    private static final String ALGORITHM = "ECDH";
    private static final String PROVIDER = "BC";
    private static final int PUBKEYLENGTH = 33;

    /* Static ECDH parameter classes */
    private static final ECNamedCurveParameterSpec ecParamSpec = ECNamedCurveTable.getParameterSpec(CURVENAME);
    private static final ECCurve curve = ecParamSpec.getCurve();

    /* Status of the instance */
    private boolean destroyed = false;

    /* Main key pair properties */
    private KeyPair keypair;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag ECDH key pair
     * @throws GeneralSecurityException if the key pair could not be created
     */
    public WfECDHKeyPair() throws GeneralSecurityException {
        this.keypair = createKeyPair();
    }

    /**
     * Constructs a new Whiteflag ECDH key pair from an existing private key
     * @param ecPrivateKey the private key object
     * @throws GeneralSecurityException if the private key is invalid or the key pair could not be created
     */
    public WfECDHKeyPair(final ECPrivateKey ecPrivateKey) throws GeneralSecurityException {
        this.keypair = createKeyPair(ecPrivateKey);
    }

    /* PUBLIC METHODS */

    /**
     * Destroys this Whiteflag ECDH key pair by clearing the private key
     * @throws DestroyFailedException if the destroy operation fails
     * @throws IllegalStateException if the encryption key has already been destroyed
     */
    @Override
    public final void destroy() throws DestroyFailedException {
        keypair.getPrivate().destroy();    // Destroy derived key; throws exceptions
        this.destroyed = true;
    }

    /**
     * Determine if this Whiteflag cipher has been destroyed.
     * @return TRUE if destroyed, else FALSE
     */
    @Override
    public final boolean isDestroyed() {
        return destroyed;
    }

    /**
     * Returns the public key of this key pair
     * @return a public key object
     * @throws IllegalStateException if the key pair has been destroyed
     */
    public final ECPublicKey getPublicKey() {
        checkState();
        return (ECPublicKey) keypair.getPublic();
    }

    /**
     * Returns the raw public key of the ECDH key pair
     * @return a byte array with the raw 264-bit compressed public ECDH key
     * @throws IllegalStateException if the key pair has been destroyed
     */
    public final byte[] getRawPublicKey() {
        checkState();
        return compressPublicKey(getPublicKey());
    }

    /**
     * Calculates the negotiated shared key with an originator
     * @param rawPublicKey the originator's raw 264-bit compressed public ECDH key
     * @return a byte array with the negotiated secret key
     * @throws WfCryptoException if the raw key or any of the parameters is invalid
     * @throws IllegalStateException if the key pair has been destroyed
     */
    public final byte[] negotiateKey(final byte[] rawPublicKey) throws WfCryptoException {
        try {
            return negotiateKey(createPublicKey(rawPublicKey));
        } catch (GeneralSecurityException e) {
            throw new WfCryptoException("Cannot create negotiated key from public key: " + e.getMessage());
        }
    }

    /**
     * Calculates the negotiated shared key with an originator
     * @param ecPublicKey the originator's ECDH public key
     * @return a byte array with the negotiated secret key
     * @throws WfCryptoException if the raw key or any of the parameters is invalid
     * @throws IllegalStateException if the key pair has been destroyed
     */
    public final byte[] negotiateKey(final ECPublicKey ecPublicKey) throws WfCryptoException {
        checkState();
        try {
            KeyAgreement ka = KeyAgreement.getInstance(ALGORITHM, PROVIDER);
            ka.init(keypair.getPrivate());
            ka.doPhase(ecPublicKey, true);
            return ka.generateSecret();
        } catch (GeneralSecurityException e) {
            throw new WfCryptoException("Cannot create negotiated key from public key: " + e.getMessage());
        }
    }

    /* PUBLIC STATIC METHODS */

    /**
     * Creates a new random ECDH key pair with the curve specified for Whiteflag key negotiation
     * @return a key pair object
     * @throws GeneralSecurityException if the key pair could not be created
     */
    public static final KeyPair createKeyPair() throws GeneralSecurityException {
        KeyPairGenerator kpg = KeyPairGenerator.getInstance(ALGORITHM, PROVIDER);
        kpg.initialize(ecParamSpec);
        return kpg.generateKeyPair();
    }

    /**
     * Creates an ECDH key pair from an existing private key with the curve specified for Whiteflag key negotiation
     * @param ecPrivateKey the ECDH private key object
     * @return a key pair object
     * @throws GeneralSecurityException if the private key is invalid
     */
    public static final KeyPair createKeyPair(ECPrivateKey ecPrivateKey) throws GeneralSecurityException {
        KeyFactory kf = KeyFactory.getInstance(ALGORITHM, PROVIDER);
        ECPoint point = ecParamSpec.getG().multiply(ecPrivateKey.getS());
        ECPublicKeySpec ecPubkeySpec = getPublicKeySpec(point.getEncoded(false));
        return new KeyPair(kf.generatePublic(ecPubkeySpec), ecPrivateKey);
    }

    /**
     * Creates an ECDH public key object from a byte array
     * @param rawPublicKey a string with the raw 264-bit compressed public ECDH key
     * @return an ECDH public key object
     * @throws GeneralSecurityException if the raw key or any of the parameters is invalid
     */
    public static final ECPublicKey createPublicKey(String rawPublicKey) throws GeneralSecurityException {
        return createPublicKey(convertToByteArray(rawPublicKey));
    }

    /**
     * Creates an ECDH public key object from a byte array
     * @param rawPublicKey a byte array with the raw 264-bit compressed public ECDH key
     * @return an ECDH public key object
     * @throws GeneralSecurityException if the raw key or any of the parameters is invalid
     */
	public static final ECPublicKey createPublicKey(byte[] rawPublicKey) throws GeneralSecurityException {
        KeyFactory kf = KeyFactory.getInstance(ALGORITHM, PROVIDER);
        ECPublicKeySpec ecPubkeySpec = getPublicKeySpec(rawPublicKey);
        return (ECPublicKey) kf.generatePublic(ecPubkeySpec);
	}

    /**
     * Creates an ECDH private key object from a byte array
     * @param rawPrivateKey a byte array with the raw private ECDH key
     * @return an ECDH private key object
     * @throws GeneralSecurityException if the raw key or any of the parameters is invalid
     */
    public static final ECPrivateKey createPrivateKey(byte[] rawPrivateKey) throws GeneralSecurityException {
        KeyFactory kf = KeyFactory.getInstance(ALGORITHM, PROVIDER);
        ECPrivateKeySpec ecPrivkeySpec = new ECPrivateKeySpec(new BigInteger(rawPrivateKey), ecParamSpec);
        return (ECPrivateKey) kf.generatePrivate(ecPrivkeySpec);
    }

    /**
     * Compresses an ECDH public key to a raw 264-bit compressed public ECDH key
     * @param ecPublicKey an ECDH public key object
     * @return a byte array with the raw 264-bit compressed public ECDH key
     */
    public static final byte[] compressPublicKey(ECPublicKey ecPublicKey) {
        // Get coordinates of public key
        final BigInteger y = ecPublicKey.getW().getAffineY();
        final BigInteger x = ecPublicKey.getW().getAffineX();

        // Copy x-coordinate into byte array 
        byte[] compressedPubkey = new byte[PUBKEYLENGTH];
        byte[] xBytes = x.toByteArray();
        final int startByte = compressedPubkey.length - xBytes.length;
        System.arraycopy(xBytes, 0, compressedPubkey, startByte, xBytes.length);

        // Set first byte of compressed key and return compressed key
        if (y.testBit(0)) compressedPubkey[0] = 0x03;   // y is odd
            else compressedPubkey[0] = 0x02;            // y is even
        return compressedPubkey;
    }

    /* PRIVATE METHODS */

    /**
     * Checks the state of this key pair
     * @throws IllegalStateException if in an illegal state
     */
    private final void checkState() {
        if (destroyed) throw new IllegalStateException("Cipher has been destroyed");
    }

    /* PRIVATE STATIC METHODS */

    /**
     * Calculates the point on the curve and returns public key specification
     * @param coordinates a byte array with the ASN.1 encoded coordinates
     * @return the {@link org.bouncycastle.jce.spec.ECPublicKeySpec} with the public key specification
     */
    private static final ECPublicKeySpec getPublicKeySpec(byte[] coordinates) {
        ECPoint point = curve.decodePoint(coordinates);
        return new ECPublicKeySpec(point, ecParamSpec);
    }
}
