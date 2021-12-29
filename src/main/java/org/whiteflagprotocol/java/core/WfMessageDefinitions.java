/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

/* Field encodings required for field definitions */
import static org.whiteflagprotocol.java.core.WfMessageCodec.Encoding.*;

/**
 * Whiteflag message definitions utility class
 *
 * <p> This is a non-instantiatable utility class that holds all message and
 * message field definitions in accordance with the Whiteflag specification.
 * No implementation specific properties and methods are defined by this class.
 * 
 * @wfver v1-draft.6
 * @wfref 4. Message Format
 * 
 * @since 1.0
 */
public class WfMessageDefinitions {

    /* PROPERTIES */

    /**
     *  Undefined fields
     */
    protected static final WfMessageField[] UNDEFINED = new WfMessageField[] {};

    /**
     * Generic message header fields
     * @wfver v1-draft.6
     * @wfref 4.2.1 Generic Message Header
     */
    protected static final WfMessageField[] genericHeaderFields = new WfMessageField[] {
        WfMessageField.define("Prefix", "^WF$", UTF8, 0, 2),
        WfMessageField.define("Version", "(?=1)^[A-Z0-9]{1}$", UTF8, 2, 3),
        WfMessageField.define("EncryptionIndicator", "(?=0|1|2)^[A-Z0-9]{1}$", UTF8, 3, 4),
        WfMessageField.define("DuressIndicator", "^[0-1]{1}$", BIN, 4, 5),
        WfMessageField.define("MessageCode", "(?=A|K|T|P|E|S|D|I|M|Q|R|F)^[A-Z]{1}$", UTF8, 5, 6),
        WfMessageField.define("ReferenceIndicator", "(?=0|1|2|3|4|5|6|7|8|9)^"+HEX.charset()+"{1}$", HEX, 6, 7),
        WfMessageField.define("ReferencedMessage", "^"+HEX.charset()+"{64}$", HEX, 7, 71)
    }; 

    /**
     * Authentication message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.4 Management Messages: Authentication
     */
    protected static final WfMessageField[] authenticationBodyFields = new WfMessageField[] {
        WfMessageField.define("VerificationMethod", "(?=1|2)^"+HEX.charset()+"{1}$", HEX, 71, 72),
        WfMessageField.define("VerificationData", "^"+UTF8.charset()+"*$", UTF8, 72, -1)
    };

    /**
     * Cryptographic message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.5 Management Messages: Cryptographic Support
     */
    protected static final WfMessageField[] cryptoBodyFields = new WfMessageField[] {
        WfMessageField.define("CryptoDataType", "^"+HEX.charset()+"{2}$", HEX, 71, 73),
        WfMessageField.define("CryptoData", "^"+HEX.charset()+"*$", HEX, 73, -1)
    };

    /**
     * Free text message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.3 Functional Messages: Free Text
     */
    protected static final WfMessageField[] freetextBodyFields = new WfMessageField[] {
        WfMessageField.define("Text", "^"+UTF8.charset()+"*$", UTF8, 71, -1)
    };

    /**
     * Resource message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.2 Functional Messages: Resource
     */
    protected static final WfMessageField[] resourceBodyFields = new WfMessageField[] {
        WfMessageField.define("ResourceMethod", "(?=1)^"+HEX.charset()+"{1}$", HEX, 71, 72),
        WfMessageField.define("ResourceData", "^"+UTF8.charset()+"*$", UTF8, 72, -1)
    };

    /**
     * Test message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.6 Management Messages: Test
     */
    protected static final WfMessageField[] testBodyFields = new WfMessageField[] {
        WfMessageField.define("PseudoMessageCode", "^[A-Z]{1}$", UTF8, 71, 72)
    };

    /**
     * Sign/signal message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     */
    protected static final WfMessageField[] signsignalBodyFields = new WfMessageField[] {
        WfMessageField.define("SubjectCode", "^"+HEX.charset()+"{2}$", HEX, 71, 73),
        WfMessageField.define("DateTime", "^"+DATETIME.charset()+"$", DATETIME, 73, 93),
        WfMessageField.define("Duration", "^"+DURATION.charset()+"$", DURATION, 93, 103),
        WfMessageField.define("ObjectType", "^"+HEX.charset()+"{2}$", HEX, 103, 105),
        WfMessageField.define("ObjectLatitude", "^"+LAT.charset()+"$", LAT, 105, 114),
        WfMessageField.define("ObjectLongitude", "^"+LONG.charset()+"$", LONG, 114, 124),
        WfMessageField.define("ObjectSizeDim1", "^"+DEC.charset()+"{4}$", DEC, 124, 128),
        WfMessageField.define("ObjectSizeDim2", "^"+DEC.charset()+"{4}$", DEC, 128, 132),
        WfMessageField.define("ObjectOrientation", "^"+DEC.charset()+"{3}$", DEC, 132, 135)
    };

    /**
     * Request fields
     * @wfver v1-draft.6
     * @wfref 4.3.1.9 Object Request Fields
     */
    protected static final WfMessageField[] requestFields = new WfMessageField[] {
        WfMessageField.define("ObjectType", "^"+HEX.charset()+"{2}$", HEX, 135, 137),
        WfMessageField.define("ObjectTypeQuant", "^"+DEC.charset()+"{2}$", DEC, 137, 139)
    };

    /* CONSTRUCTOR */

    /** 
     * Prevents this utility class to be instantiated
     */
    private WfMessageDefinitions() {
        throw new IllegalStateException("Cannot instantiate Whiteflag message definitions utility class");
    }
}
