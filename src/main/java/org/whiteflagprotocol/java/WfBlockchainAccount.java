/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

import java.net.URL;
import java.net.MalformedURLException;
import java.security.GeneralSecurityException;
import java.security.KeyPair;
import java.security.PublicKey;
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

    /* Basic Properties */
    private String name;

    /* Whiteflag Authentication & Crytptography */
    /**
     * The URL used for authentication in A1 message
     */
    private URL authURL;
    /**
     * The URL used for authentication in A1 message
     */
    private WfAuthToken authToken;
    private WfEncryptionKey sharedKey;
    private WfECDHKeyPair ecdhKeyPair;
    private ECPublicKey ecdhPublicKey;

    /* CONSTRUCTORS */

    /**
     * Constructs a new participant with the specified name
     * @param name the name of the participant
     */
    public WfBlockchainAccount(final String name) {
        setName(name);
    }

    /* PUBLIC METHODS */

    /**
     * Sets the name of the participant
     * @param name the name of the participant
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Gets the name of the participant
     * @return a string with the name of the participant
     */
    public String getName() {
        return this.name;
    }

    /**
     * Sets the authentication URL from an A1 message used to identify the participant
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @param url a string with an URL pointinng to the authentication data
     * @throws MalformedURLException if the string does not contain a valid URL
     */
    public void setAuthURL(final String url) throws MalformedURLException {
        this.authURL = new URL(url);
    }

    /**
     * Sets the authentication URL from an A1 message used to identify the participant
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @param url a {@link java.net.URL} pointinng to the authentication data
     */
    public void setAuthURL(final URL url) {
        this.authURL = url;
    }

    /**
     * Gets the authentication URL from an A1 message used to identify the participant
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @return a {@link java.net.URL} pointinng to the authentication data
     */
    public URL getAuthURL() {
        return this.authURL;
    }

    /**
     * Sets the authentication token used to identify the participant by an A2 message
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @param token a {@link org.whiteflagprotocol.java.crypto.WfAuthToken} authentication token
     */
    public void setAuthToken(final WfAuthToken token) {
        this.authToken = token;
    }

    /**
     * Sets the authentication token used to identify the participant by an A2 message
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @return the {@link org.whiteflagprotocol.java.crypto.WfAuthToken} authentication token
     */
    public WfAuthToken getAuthToken() {
        return this.authToken;
    }

    /**
     * Sets the shared encryption key for this participant
     * @wfref 5.2.4 Message Encryption
     * @param key a pre-shared {@link org.whiteflagprotocol.java.crypto.WfEncryptionKey} encryption key
     */
    public void setSharedKey(final WfEncryptionKey key) {
        this.sharedKey = key;
    }

    /**
     * Gets the shared encryption key for this participant
     * @wfref 5.2.4 Message Encryption
     * @return the pre-shared {@link org.whiteflagprotocol.java.crypto.WfEncryptionKey} encryption key
     */
    public WfEncryptionKey getSharedKey() {
        return this.sharedKey;
    }

    /**
     * Sets the ECDH key pair used to derrive a negotiated key with another participant (typically used if an instance represents)
     * @wfref 5.2.4 Message Encryption
     * @param ecdhKeyPair a {@link org.whiteflagprotocol.java.crypto.WfECDHKeyPair} ECDH key pair
     */
    public void setEcdhKeyPair(final WfECDHKeyPair ecdhKeyPair) {
        this.ecdhKeyPair = ecdhKeyPair;
    }

    /**
     * Gets the ECDH public key used to derrive the negotiated key with this participant
     * @wfref 5.2.4 Message Encryption
     * @return the {@link java.security.interfaces.ECPublicKey} ECDH public key
     */
    public WfECDHKeyPair getEcdhKeyPair() {
        return this.ecdhKeyPair;
    }

    /**
     * Sets the ECDH public key used to derrive the negotiated key with this participant
     * @wfref 5.2.4 Message Encryption
     * @param rawPublicKey a string with the raw 264-bit compressed public ECDH key
     * @throws WfException if the provided raw public key is invalid
     */
    public void setEcdhPublicKey(final String rawPublicKey) throws WfException {
        try {
            this.setEcdhPublicKey(WfECDHKeyPair.createPublicKey(rawPublicKey));
        } catch(GeneralSecurityException e) {
            throw new WfException("Cannot accept raw ECDH public key: " + e.getLocalizedMessage(), ErrorType.WF_ENCRYPTION_ERROR);
        }
    }

    /**
     * Sets the ECDH public key used to derrive the negotiated key with this participant
     * @wfref 5.2.4 Message Encryption
     * @param ecdhPublicKey a {@link java.security.interfaces.ECPublicKey} ECDH public key
     */
    public void setEcdhPublicKey(final ECPublicKey ecdhPublicKey) {
        this.ecdhPublicKey = ecdhPublicKey;
    }

    /**
     * Gets the ECDH public key used to derrive the negotiated key with this participant
     * @wfref 5.2.4 Message Encryption
     * @return the {@link java.security.interfaces.ECPublicKey} ECDH public key
     */
    public ECPublicKey getEcdhPublicKey() {
        return this.ecdhPublicKey;
    }
}
