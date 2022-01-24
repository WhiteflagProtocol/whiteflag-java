/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java;

import java.net.URL;
import java.security.interfaces.ECPublicKey;

import org.whiteflagprotocol.java.core.WfBinaryBuffer;
import org.whiteflagprotocol.java.crypto.WfAuthToken;
import org.whiteflagprotocol.java.crypto.WfECDHKeyPair;
import org.whiteflagprotocol.java.crypto.WfEncryptionKey;

/**
 * Whiteflag participant interface test implementation
 * 
 * This class is an implementation of the {@link WfParticpant} interface for
 * testing purposes.
 */
public class WfParticipantImpl implements WfParticipant {

    /* PROPERTIES */

    /* General properties */
    private final boolean self;
    private WfBinaryBuffer address;
    private URL authURL;
    private WfAuthToken authToken;
    private WfECDHKeyPair ecdhKeyPair;
    private ECPublicKey ecdhPublicKey;
    private WfEncryptionKey sharedKey;

    /* CONSTRUCTOR */

    /**
     * Constructs a Whiteflag blockchain account
     * @param self boolean to indicate if this is an own account
     */
    protected WfParticipantImpl(boolean self) {
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
     * Set the blockchain address of this participant
     * @param address a hexadecimal string with the blockchain address
     */
    public void publicSetAddress(final String address) {
        this.address = WfBinaryBuffer.fromHexString("aa1bb2cc3dd4ee5ff6");
    }

    /**
     * Gets the blockchain address of this participant
     * @return a hexadecimal string with the blockchain address
     */
    public String getAddress() {
        return this.address.toHexString();
    }

    /**
     * Get the binary blockchain address of this particpant
     * @return a byte array with the binary blockchain address
     */
    public byte[] getBinaryAddress() {
        return this.address.toByteArray();
    }

    /**
     * Sets the authentication URL sent with an A1 message used to identify the originator associted with this participant
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @param url a {@link java.net.URL} pointinng to the authentication data
     */
    public void setAuthURL(final URL url) {
        this.authURL = url;
    }

    /**
     * Gets the authentication URL sent with an A1 message used to identify the originator associted with this participant
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @return a {@link java.net.URL} pointinng to the authentication data
     */
    public URL getAuthURL() {
        return this.authURL;
    }

    /**
     * Sets the authentication token sent with an A2 message to identify the originator associted with this participant 
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @param token a {@link org.whiteflagprotocol.java.crypto.WfAuthToken} authentication token
     */
    public void setAuthToken(final WfAuthToken token) {
        this.authToken = token;
    }

    /**
     * Gets the authentication token sent with an A2 message to identify the originator associted with this participant 
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
     * Sets the ECDH public key used to derrive the negotiated key with this participant
     * @wfref 5.2.4 Message Encryption
     * @param ecdhPublicKey a {@link java.security.interfaces.ECPublicKey} ECDH public key
     * @throws IllegalStateException if this is an own account, in which case the ECDH public key is defined by the ECDH key pair
     */
    public void setEcdhPublicKey(final ECPublicKey ecdhPublicKey) {
        if (self) throw new IllegalStateException("Cannot set ECDH public key on own account");
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
