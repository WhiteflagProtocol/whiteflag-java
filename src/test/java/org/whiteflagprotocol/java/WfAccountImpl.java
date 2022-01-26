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
 * Whiteflag account interface test implementation
 * 
 * This class is an implementation of the {@link WfParticpant} interface for
 * testing purposes.
 */
public class WfAccountImpl implements WfAccount {

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
    protected WfAccountImpl(boolean self) {
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
     * Set the blockchain address that corresponds with this account
     * @param address a hexadecimal string with the blockchain address
     */
    public void setAddress(final String address) {
        this.address = WfBinaryBuffer.fromHexString(address);
    }

    /**
     * Gets the blockchain address that corresponds with this account
     * @return a hexadecimal string with the blockchain address
     * @throws IllegalStateException if no blockchain address available
     */
    public String getAddress() {
        if (this.address == null) return null;
        return this.address.toHexString();
    }

    /**
     * Get the binary blockchain address that corresponds with this account
     * @return a byte array with the binary blockchain address
     * @throws IllegalStateException if no blockchain address available
     */
    public byte[] getBinaryAddress() {
        if (this.address == null) throw new IllegalStateException("No blockchain address available");
        return this.address.toByteArray();
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
     * Gets the authentication URL sent with an A1 message used to identify the originator associated with this account
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @return a {@link java.net.URL} pointinng to the authentication data
     * @throws IllegalStateException if no authentication URL available
     */
    public URL getAuthURL() {
        if (this.authURL == null) throw new IllegalStateException("No authentication URL available");
        return this.authURL;
    }

    /**
     * Sets the authentication token sent with an A2 message to identify the originator associated with this account 
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @param token a {@link org.whiteflagprotocol.java.crypto.WfAuthToken} authentication token
     */
    public void setAuthToken(final WfAuthToken token) {
        this.authToken = token;
    }

    /**
     * Gets the authentication token sent with an A2 message to identify the originator associated with this account 
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @return the {@link org.whiteflagprotocol.java.crypto.WfAuthToken} authentication token
     * @throws IllegalStateException if no authentication token available
     */
    public WfAuthToken getAuthToken() {
        if (this.authToken == null) throw new IllegalStateException("No authentication token available");
        return this.authToken;
    }

    /**
     * Sets the shared encryption key with this participant's account
     * @wfref 5.2.4 Message Encryption
     * @param key a pre-shared {@link org.whiteflagprotocol.java.crypto.WfEncryptionKey} encryption key
     */
    public void setSharedKey(final WfEncryptionKey key) {
        this.sharedKey = key;
    }

    /**
     * Gets the shared encryption key with this participant's account
     * @wfref 5.2.4 Message Encryption
     * @return the pre-shared {@link org.whiteflagprotocol.java.crypto.WfEncryptionKey} encryption key
     * @throws IllegalStateException if no pre-shared key available
     */
    public WfEncryptionKey getSharedKey() {
        if (this.sharedKey == null) throw new IllegalStateException("No pre-shared key available");
        return this.sharedKey;
    }

    /**
     * Sets the own ECDH key pair used to derrive a negotiated key with another originator
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
     * @throws IllegalStateException if no ECDH key pair available
     */
    public WfECDHKeyPair getEcdhKeyPair() {
        if (this.ecdhKeyPair == null) throw new IllegalStateException("No ECDH key pair available");
        return this.ecdhKeyPair;
    }

    /**
     * Sets the ECDH public key used to derrive the negotiated key with this participant's account
     * @wfref 5.2.4 Message Encryption
     * @param ecdhPublicKey a {@link java.security.interfaces.ECPublicKey} ECDH public key
     * @throws IllegalStateException if this is an own account, in which case the ECDH public key is defined by the ECDH key pair
     */
    public void setEcdhPublicKey(final ECPublicKey ecdhPublicKey) {
        if (self) throw new IllegalStateException("Cannot set ECDH public key on own account");
        this.ecdhPublicKey = ecdhPublicKey;
    }

    /**
     * Gets the ECDH public key used to derrive the negotiated key with this participant's account
     * @wfref 5.2.4 Message Encryption
     * @return the {@link java.security.interfaces.ECPublicKey} ECDH public key
     * @throws IllegalStateException if no ECDH public key available
     */
    public ECPublicKey getEcdhPublicKey() {
        if (this.ecdhPublicKey == null) throw new IllegalStateException("No ECDH public key available");
        return this.ecdhPublicKey;
    }
}
