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
 * This class is an implementation of the {@link WfAccount} interface for
 * testing purposes.
 */
public class WfDummyAccount implements WfAccount {

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
    protected WfDummyAccount(boolean self) {
        this.self = self;
    }
    
    /* PUBLIC METHODS */

    /**
     * Checks if this is an own account
     * @return TRUE if own accopunt, else FALSE if associated wiht another originator
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
     * @return a hexadecimal string with the blockchain address, or null if not available
     */
    public String getAddress() {
        if (this.address == null) return null;
        return this.address.toHexString();
    }

    /**
     * Get the binary blockchain address that corresponds with this account
     * @return a byte array with the binary blockchain address
     */
    public byte[] getBinaryAddress() {
        if (this.address == null) return new byte[0];
        return this.address.toByteArray();
    }

    /**
     * Sets the authentication URL sent with an A1 message used to identify the originator associated with this account
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @param url a URL pointing to the authentication data
     */
    public void setAuthURL(final URL url) {
        this.authURL = url;
    }

    /**
     * Gets the authentication URL sent with an A1 message used to identify the originator associated with this account
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @return a URL pointing to the authentication data, or null if not available
     */
    public URL getAuthURL() {
        return this.authURL;
    }

    /**
     * Sets the authentication token sent with an A2 message to identify the originator associated with this account 
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @param token an authentication token
     */
    public void setAuthToken(final WfAuthToken token) {
        this.authToken = token;
    }

    /**
     * Gets the authentication token sent with an A2 message to identify the originator associated with this account 
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @return the authentication token, or null if not available
     */
    public WfAuthToken getAuthToken() {
        return this.authToken;
    }

    /**
     * Sets the shared encryption key with this participant's account
     * @wfref 5.2.4 Message Encryption
     * @param key a pre-shared encryption key
     */
    public void setSharedKey(final WfEncryptionKey key) {
        this.sharedKey = key;
    }

    /**
     * Gets the shared encryption key with this participant's account
     * @wfref 5.2.4 Message Encryption
     * @return the pre-shared encryption key, or null if not available
     */
    public WfEncryptionKey getSharedKey() {
        return this.sharedKey;
    }

    /**
     * Sets the own ECDH key pair used to derrive a negotiated key with another originator
     * @wfref 5.2.4 Message Encryption
     * @param ecdhKeyPair an ECDH key pair
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
     * @return the ECDH public key, or null if not available
     */
    public WfECDHKeyPair getEcdhKeyPair() {
        return this.ecdhKeyPair;
    }

    /**
     * Sets the ECDH public key used to derrive the negotiated key with this participant's account
     * @wfref 5.2.4 Message Encryption
     * @param ecdhPublicKey an ECDH public key
     * @throws IllegalStateException if this is an own account, in which case the ECDH public key is defined by the ECDH key pair
     */
    public void setEcdhPublicKey(final ECPublicKey ecdhPublicKey) {
        if (self) throw new IllegalStateException("Cannot set ECDH public key on own account");
        this.ecdhPublicKey = ecdhPublicKey;
    }

    /**
     * Gets the ECDH public key used to derrive the negotiated key with this participant's account
     * @wfref 5.2.4 Message Encryption
     * @return the ECDH public key, or null if not available
     */
    public ECPublicKey getEcdhPublicKey() {
        return this.ecdhPublicKey;
    }
}
