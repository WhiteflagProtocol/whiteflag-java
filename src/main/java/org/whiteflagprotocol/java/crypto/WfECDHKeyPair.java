/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import java.math.BigInteger;
import java.security.AlgorithmParameters;
import java.security.GeneralSecurityException;
import java.security.InvalidAlgorithmParameterException;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.interfaces.ECPublicKey;
import java.security.spec.ECParameterSpec;
import java.security.spec.ECGenParameterSpec;
import java.security.spec.ECPoint;
import java.security.spec.ECPublicKeySpec;
import java.security.spec.InvalidParameterSpecException;

import javax.crypto.KeyAgreement;

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
public class WfECDHKeyPair {

    /* PROPERTIES */

    /* Constants */
    private static final String KEYAGREEMENTALG = "DH";
    private static final String KEYPAIRTYPE = "EC";
    private static final String CURVE = "brainpoolP256R1";
    
    /* ECDH Paramaters */
    private static final ECGenParameterSpec ecParametersSpec = new ECGenParameterSpec(CURVE);   //TODO: Check curve provider
    private final AlgorithmParameters ecParametersProvider = AlgorithmParameters.getInstance(KEYPAIRTYPE);
    private final ECParameterSpec ecParameters;

    /* Main key pair properties */
    KeyPair keyPair;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag ECDH key pair
     */
    public WfECDHKeyPair() throws NoSuchAlgorithmException, InvalidParameterSpecException, InvalidAlgorithmParameterException {
        // Set the correct EC curve parameters
        this.ecParametersProvider.init(ecParametersSpec);
        this.ecParameters = ecParametersProvider.getParameterSpec(ECParameterSpec.class);

        // Generate key pair
        KeyPairGenerator kpg = KeyPairGenerator.getInstance(KEYPAIRTYPE);
        kpg.initialize(ecParameters);
        this.keyPair = kpg.generateKeyPair();
    }

    /* PUBLIC METHODS */

    /**
     * Returns the public key of the ECDH key pair
     * @return a byte array with the ECDH public key
     */
    public final byte[] getPublicKey() {
        return keyPair.getPublic().getEncoded();  // WIP
    }

    /**
     * Calculates the shared secret with an originator
     * @param originatorPublicKey the originator's raw 264-bit compressed public ECDH key
     * @return a byte array with the shared secret
     */
    public final byte[] getSharedKey(final byte[] originatorPublicKey) throws GeneralSecurityException {
        KeyAgreement ka = KeyAgreement.getInstance(KEYAGREEMENTALG);
        ka.init(keyPair.getPrivate());
        ka.doPhase(createPublicKey(originatorPublicKey), true);
        return ka.generateSecret();
    }

    /**
     * Creates a ECDH public key object from a byte array
     * @param rawPublicKey a byte array with the raw 264-bit compressed public ECDH key
     * @throws WfCryptoException if the raw key or any of the paramters is invalid
     */
    protected ECPublicKey createPublicKey(byte[] rawPublicKey) throws GeneralSecurityException {
        KeyFactory kf = KeyFactory.getInstance(KEYAGREEMENTALG);
        ECPoint w = new ECPoint(BigInteger.valueOf(0), BigInteger.valueOf(0));  //TODO: Calculate point from Y coordinate
        ECPublicKeySpec keyspec = new ECPublicKeySpec(w, ecParameters);
        return (ECPublicKey) kf.generatePublic(keyspec);
    }
}
