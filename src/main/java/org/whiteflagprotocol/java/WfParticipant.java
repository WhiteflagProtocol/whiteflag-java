package org.whiteflagprotocol.java;

import java.net.URL;
import java.security.interfaces.ECPublicKey;

import org.whiteflagprotocol.java.crypto.WfAuthToken;
import org.whiteflagprotocol.java.crypto.WfECDHKeyPair;
import org.whiteflagprotocol.java.crypto.WfEncryptionKey;

/**
 * Whiteflag particpant interface
 * 
 * This interface defines how a class representing a particpiant in Whiteflag
 * communications must interact with WFJL classes that need information about
 * that particpant. Participants are originators and recipients of Whiteflag
 * messages, which may be (groups of) organisations and individuals, that have
 * attributes such as blockchain addresses, authentication information and
 * encryption keys. This interface defines how these attributes must be
 * provided to instances of classes such a {@link WfMessage}. Note that methods
 * of this interface may return `null` if certain data is unknown or does not
 * exist for a particpant.
 *  
 * @since 1.1
 */
public interface WfParticipant {

    /**
     * Checks if this participant represents yourself
     * @return TRUE if self, else FALSE if participant is somebody else
     */
    public boolean isSelf();

    /**
     * Returns the blockchain address as a string in the encoding specified for that specific blockchain
     * 
     * <p> Blockchain addresses are encoded differently for different
     * blockchains. For example, Bitcoin uses a modified Base58 encoding
     * and Ethereum uses a hexadecimal string.
     * 
     * @return a string with the blockchain address in the encoding used for that blockchain
     */
    public String getAddress();

    /**
     * Provides the binary blockchain address indepent from the representation for that blockchain
     * 
     * <p> Although addresses of diffrent blockchains are encoded differently,
     * they all are essentially a binary value. Whiteflag uses that binary
     * value to bind cryptographic keys and tokens to an address.
     * 
     * @return a byte array with the binary blockchain address
     */
    public byte[] getBinaryAddress();

    /**
     * Gets the authentication URL sent with an A1 message used to identify the participant as an originator
     * @wfref 5.1.2.1 Method 1: URL Validation
     * @return a {@link java.net.URL} pointinng to the authentication data, or null if no known url
     */
    public URL getAuthURL();

    /**
     * Gets the authentication token sent with an A2 message to identify the participant as an originator
     * @wfref 5.1.2.2 Method 2: Shared Token Validation
     * @return the {@link org.whiteflagprotocol.java.crypto.WfAuthToken} authentication token, or null if no knwon token
     */
    public WfAuthToken getAuthToken();

    /**
     * Gets the shared encryption key qith this participant
     * @wfref 5.2.4 Message Encryption
     * @return the pre-shared {@link org.whiteflagprotocol.java.crypto.WfEncryptionKey} encryption key, or null if no key available
     */
    public WfEncryptionKey getSharedKey();


    /**
     * Gets the own ECDH key pair used to negatiate keys with others
     * @wfref 5.2.4 Message Encryption
     * @return the {@link java.security.interfaces.ECPublicKey} ECDH public key, or null if no key pair available
     */
    public WfECDHKeyPair getEcdhKeyPair();

    /**
     * Gets the other's ECDH public key used to negatioate a key
     * @wfref 5.2.4 Message Encryption
     * @return the {@link java.security.interfaces.ECPublicKey} ECDH public key, or null is no public key available
     */
    public ECPublicKey getEcdhPublicKey();    
}
