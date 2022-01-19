/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

import java.net.URL;
import java.net.MalformedURLException;
import java.security.GeneralSecurityException;
import java.security.interfaces.ECPublicKey;

import org.whiteflagprotocol.java.WfException.ErrorType;
import org.whiteflagprotocol.java.crypto.WfAuthToken;
import org.whiteflagprotocol.java.crypto.WfECDHKeyPair;
import org.whiteflagprotocol.java.crypto.WfEncryptionKey;

/**
 * Whiteflag account abstract class
 * 
 * <p> This class represents a blockchain account to or from which Whiteflag
 * messages are sent. It may correspond with an actual blockchain account for
 * blockchains that use accounts. For bockchains that lack the concept of
 * accounts, such as Bitcoin, instances of this account class must be seen as
 * a virtual account, i.e. a container to hold a Whiteflag originator's
 * authentication data and encryption keys.
 * 
 * This class is an abstract class, because its actual implementation depends
 * on the blockchain (e.g.key pair, address, etc.). The Whiteflag specification
 * assumes that a blockchain account is identified by a blockchain address;
 * therefore, this class implements the {@link WfAddress} interface.
 * 
 * @since 1.1
 */
abstract class WfBlockchainAccount implements WfBlockchainAddress {

    /* PROPERTIES */

    /* General properties */
    /**
     * Indicator if this is an own account
     */
    private final boolean self;

    /* Whiteflag Authentication & Crytptography */
    /**
     * The URL for Authentication Method 1
     */
    private URL authURL;
    /**
     * The token for Authentication Method 2
     */
    private WfAuthToken authToken;
    /**
     * The ECDH keys used to negotiate the key for Encryption Method 1
     */
    private WfECDHKeyPair ecdhKeyPair;
    private ECPublicKey ecdhPublicKey;
    /**
     * The encryption key for Encryption Method 2
     */
    private WfEncryptionKey sharedKey;

    /* CONSTRUCTOR */

    /**
     * Constructs a Whiteflag blockchain account
     * @param self boolean to indicate if this is an own account
     */
    protected WfBlockchainAccount(boolean self) {
        this.self = self;
    }
    
    /* PUBLIC METHODS */

    /**
     * Checks if this is an own account
     * @return TRUE if own accopunt, else if associated wiht another originator
     */
    public boolean isSelf() {
        return self;
    }

    /**
     * Sets the authentication URL sent with an A1 message used to identify the originator associted with this account
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @param url a string with an URL pointinng to the authentication data
     * @throws MalformedURLException if the string does not contain a valid URL
     */
    public void setAuthURL(final String url) throws MalformedURLException {
        this.authURL = new URL(url);
    }

    /**
     * Sets the authentication URL sent with an A1 message used to identify the originator associted with this account
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @param url a {@link java.net.URL} pointinng to the authentication data
     */
    public void setAuthURL(final URL url) {
        this.authURL = url;
    }

    /**
     * Gets the authentication URL sent with an A1 message used to identify the originator associted with this account
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @return a {@link java.net.URL} pointinng to the authentication data
     */
    public URL getAuthURL() {
        return this.authURL;
    }

    /**
     * Sets the authentication token sent with an A2 message to identify the originator associted with this account 
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @param token a {@link org.whiteflagprotocol.java.crypto.WfAuthToken} authentication token
     */
    public void setAuthToken(final WfAuthToken token) {
        this.authToken = token;
    }

    /**
     * Gets the authentication token sent with an A2 message to identify the originator associted with this account 
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @return the {@link org.whiteflagprotocol.java.crypto.WfAuthToken} authentication token
     */
    public WfAuthToken getAuthToken() {
        return this.authToken;
    }

    /**
     * Constructs and sets a new Whiteflag encryption key from a raw pre-shared key
     * @param rawSharedKey a hexadecimal string with the raw pre-shared encryption key
     */
    public void setSharedKey(String rawSharedKey) {
        setSharedKey(new WfEncryptionKey(rawSharedKey));
    }

    /**
     * Constructs and sets a new Whiteflag encryption key from a raw pre-shared key
     * @param rawSharedKey a byte array with the raw pre-shared encryption key
     */
    public void setSharedKey(byte[] rawSharedKey) {
        setSharedKey(new WfEncryptionKey(rawSharedKey));
    }

    /**
     * Sets the shared encryption key for this account
     * @wfref 5.2.4 Message Encryption
     * @param key a pre-shared {@link org.whiteflagprotocol.java.crypto.WfEncryptionKey} encryption key
     */
    public void setSharedKey(final WfEncryptionKey key) {
        this.sharedKey = key;
    }

    /**
     * Gets the shared encryption key for this account
     * @wfref 5.2.4 Message Encryption
     * @return the pre-shared {@link org.whiteflagprotocol.java.crypto.WfEncryptionKey} encryption key
     */
    public WfEncryptionKey getSharedKey() {
        return this.sharedKey;
    }

    /**
     * Sets the ECDH key pair used to derrive a negotiated key with another originator
     * @wfref 5.2.4 Message Encryption
     * @param ecdhKeyPair a {@link org.whiteflagprotocol.java.crypto.WfECDHKeyPair} ECDH key pair
     * @throws IllegalStateException if this is not an own account
     */
    public void setEcdhKeyPair(final WfECDHKeyPair ecdhKeyPair) {
        if (!self) throw new IllegalStateException("Cannot set ECDH key pair on other's account");
        this.ecdhKeyPair = ecdhKeyPair;
        this.ecdhPublicKey = ecdhKeyPair.getPublicKey();
    }

    /**
     * Gets the ECDH public key used to derrive the negotiated key with this originator
     * @wfref 5.2.4 Message Encryption
     * @return the {@link java.security.interfaces.ECPublicKey} ECDH public key
     */
    public WfECDHKeyPair getEcdhKeyPair() {
        return this.ecdhKeyPair;
    }

    /**
     * Sets the ECDH public key used to derrive the negotiated key with this account
     * @wfref 5.2.4 Message Encryption
     * @param rawPublicKey a string with the raw 264-bit compressed public ECDH key
     * @throws WfException if the provided raw public key is invalid
     */
    public void setEcdhPublicKey(final String rawPublicKey) throws WfException {
        try {
            setEcdhPublicKey(WfECDHKeyPair.createPublicKey(rawPublicKey));
        } catch(GeneralSecurityException e) {
            throw new WfException("Cannot process raw ECDH public key: " + e.getLocalizedMessage(), ErrorType.WF_CRYPTO_ERROR);
        }
    }

    /**
     * Sets the ECDH public key used to derrive the negotiated key with this account
     * @wfref 5.2.4 Message Encryption
     * @param ecdhPublicKey a {@link java.security.interfaces.ECPublicKey} ECDH public key
     * @throws IllegalStateException if this is an own account, in which case the ECDH public key is defined by the ECDH key pair
     */
    public void setEcdhPublicKey(final ECPublicKey ecdhPublicKey) {
        if (self) throw new IllegalStateException("Cannot set ECDH public key on own account");
        this.ecdhPublicKey = ecdhPublicKey;
    }

    /**
     * Gets the ECDH public key used to derrive the negotiated key with this account
     * @wfref 5.2.4 Message Encryption
     * @return the {@link java.security.interfaces.ECPublicKey} ECDH public key
     */
    public ECPublicKey getEcdhPublicKey() {
        return this.ecdhPublicKey;
    }
}
